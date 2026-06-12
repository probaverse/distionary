#' Evaluate Quantiles from a CDF
#'
#' Pulls together all the pieces needed to calculate the left inverse
#' of the CDF (the quantile function). Intended for internal use only.
#'
#' When the distribution carries a structured support (see [support()]), a fast
#' vectorized algorithm is used: a single bisection advances every requested
#' probability at once, the support hull supplies the boundary quantiles
#' (`p == 0` and `p == 1`) directly, and any probability that lands inside an
#' atom's CDF jump is returned as that atom *exactly*. Distributions defined
#' with a legacy `.vtype` string (no structured support) fall back to the older
#' per-probability bisection in `quantile_legacy()`, which is restricted to
#' continuous distributions (atoms cannot be located without a support).
#'
#' @param distribution A distribution having access to a cdf.
#' @param at A vector of values for which to evaluate the quantile function.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number
#' of iterations (at least 1); length 1 vectors.
#' @returns The `at`-quantiles of the distribution. Numeric vector the same
#' length as `at`.
#' @noRd
eval_quantile_from_network <- function(distribution,
                                       at,
                                       tol = 1e-9,
                                       maxiter = 200) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at, 0, 1)
  checkmate::assert_numeric(tol, 0, len = 1)
  checkmate::assert_integerish(maxiter, lower = 1, len = 1)
  if (length(at) == 0) {
    return(numeric(0L))
  }
  s <- support(distribution)
  if (is.null(s)) {
    return(quantile_legacy(distribution, at, tol = tol, maxiter = maxiter))
  }
  quantile_from_support(distribution, s, at, tol = tol, maxiter = maxiter)
}

#' Quantiles via a Structured Support
#'
#' The fast path used when a distribution has a structured support. Boundary
#' probabilities come from the support hull; interior probabilities are solved
#' by a vectorized bisection and then snapped onto atoms where they fall inside
#' a CDF jump.
#'
#' @param s The distribution's support (a `support` object, not `NULL`).
#' @inheritParams eval_quantile_from_network
#' @returns The `at`-quantiles, a numeric vector the same length as `at`.
#' @noRd
quantile_from_support <- function(distribution, s, at, tol, maxiter) {
  n <- length(at)
  out <- rep(NaN, n)
  # Preserve NA (and NaN) inputs as-is; `at` is otherwise within [0, 1].
  out[is.na(at)] <- at[is.na(at)]
  ok <- !is.na(at)
  hull <- support_hull(s)
  is_zero <- ok & at == 0
  is_one <- ok & at == 1
  is_interior <- ok & at > 0 & at < 1
  # The boundary quantiles are the ends of the support: Q(0) is the infimum of
  # the support, Q(1) the supremum. Reading them from the hull (rather than
  # bisecting into the numerical tails) gives the exact endpoints, including
  # -Inf / Inf for unbounded supports. We read the support *directly* and never
  # call range(), which would recurse back here for legacy distributions.
  out[is_zero] <- hull[1L]
  out[is_one] <- hull[2L]
  if (any(is_interior)) {
    p <- at[is_interior]
    br <- quantile_bracket(distribution, hull, p)
    sol <- bisect_quantile(
      distribution, p,
      lo = br[["lo"]], hi = br[["hi"]], tol = tol, maxiter = maxiter
    )
    value <- sol[["value"]]
    if (discretes::num_discretes(s[["atoms"]]) > 0) {
      value <- snap_to_atoms(
        distribution, s[["atoms"]], p,
        lo = sol[["lo"]], hi = sol[["hi"]], value = value
      )
    }
    out[is_interior] <- value
  }
  out
}

#' Bracket the Interior Quantiles
#'
#' Finds a single `[lo, hi]` interval guaranteed to contain every interior
#' quantile, with `F(lo) < min(p)` and `F(hi) >= max(p)` so the left-inverse
#' bisection invariant holds for all probabilities. Finite support endpoints are
#' used as-is; infinite ones are replaced by an outward search that doubles away
#' from the support until the CDF clears the probability range.
#'
#' @param hull Length-2 numeric `c(lower, upper)` support hull.
#' @param p Vector of interior probabilities (strictly between 0 and 1).
#' @returns A list with scalar entries `lo` and `hi`, recycled to the length of
#' `p` by the caller.
#' @inheritParams eval_quantile_from_network
#' @noRd
quantile_bracket <- function(distribution, hull, p) {
  p_min <- min(p)
  p_max <- max(p)
  left <- if (is.finite(hull[1L])) hull[1L] else -1
  # Move below the support until F(left) < p_min. For a finite hull with an atom
  # at the lower endpoint, F(lower) can exceed p_min, so we still step down (no
  # mass lives below the support, so the CDF drops to 0 there). For negative
  # `left`, subtracting its magnitude doubles it, matching the classic outward
  # search; for a positive finite endpoint it walks down toward 0 and beyond.
  while (eval_cdf(distribution, left) >= p_min) {
    if (left == 0) {
      left <- -1
    } else {
      left <- left - max(1, abs(left))
    }
    if (is.infinite(left)) {
      left <- -.Machine$double.xmax
      break
    }
  }
  right <- if (is.finite(hull[2L])) hull[2L] else 1
  while (eval_cdf(distribution, right) < p_max) {
    if (right == 0) {
      right <- 1
    } else {
      right <- right + max(1, abs(right))
    }
    if (is.infinite(right)) {
      right <- .Machine$double.xmax
      break
    }
  }
  np <- length(p)
  list(lo = rep(left, np), hi = rep(right, np))
}

#' Vectorized Left-Inverse Bisection
#'
#' Solves the left inverse of the CDF for an entire vector of probabilities at
#' once. The bracket invariant `F(lo) < p <= F(hi)` is maintained per element;
#' each iteration evaluates the CDF a single (vectorized) time, at the
#' midpoints, and moves the appropriate endpoint of every element together.
#'
#' Convergence is on the *x-width* `hi - lo` falling below a combined
#' absolute/relative tolerance `tol * max(1, |lo|, |hi|)`, which targets a fixed
#' precision in the quantile itself (the returned x-value) and behaves well
#' across tiny to very large magnitudes.
#'
#' Elements over a flat part of the CDF (a continuous gap, or the plateau above
#' an atom) shrink their x-width normally; an exact atom is then recovered by
#' the trailing snap. The midpoint guard (`mid > lo & mid < hi`) is a
#' floating-point floor so the loop always terminates even if `tol` is below the
#' representable spacing.
#'
#' @param p Vector of probabilities, each strictly between 0 and 1.
#' @param lo,hi Numeric vectors (same length as `p`) bracketing the solutions.
#' @returns A list with `value` (the midpoint estimate), `lo`, and `hi`, each a
#' numeric vector the same length as `p`.
#' @inheritParams eval_quantile_from_network
#' @noRd
bisect_quantile <- function(distribution, p, lo, hi, tol, maxiter) {
  for (i in seq_len(maxiter)) {
    mid <- (lo + hi) / 2
    x_tol <- tol * pmax(1, abs(lo), abs(hi))
    active <- (hi - lo) > x_tol & mid > lo & mid < hi
    if (!any(active)) {
      break
    }
    f_mid <- eval_cdf(distribution, mid)
    go_left <- p <= f_mid
    move_hi <- active & go_left
    move_lo <- active & !go_left
    hi[move_hi] <- mid[move_hi]
    lo[move_lo] <- mid[move_lo]
  }
  list(value = (lo + hi) / 2, lo = lo, hi = hi)
}

#' Snap Quantiles onto Atoms
#'
#' Where the quantile of `p` is an atom `a`, the bisection only *approaches*
#' `a`; worse, common discrete CDFs (e.g. `ppois()`) fuzz their input by ~1e-7,
#' so the bracket can converge a hair below the true atom. This replaces such
#' estimates with the atom exactly. For each probability the two nearest atoms
#' (the largest `<= value` and the smallest `>= value`) are candidates, and one
#' is accepted precisely when `p` lies inside its CDF jump,
#' `F(a) - pmf(a) < p <= F(a)`. That jump test is exact and the jumps are
#' disjoint in `p`, so at most one candidate is ever accepted and a
#' continuous-part `p` (whose `value` is genuinely between atoms) is never
#' snapped.
#'
#' @param atoms_obj The atomic part of the support (a `discretes` object with at
#' least one atom).
#' @param p Vector of probabilities.
#' @param lo,hi Converged brackets from `bisect_quantile()` (unused directly;
#' the search is anchored on `value`, which lies within them).
#' @param value Bisection estimates, returned unchanged where no atom is hit.
#' @returns The `value` vector with atom-hitting entries replaced by the atoms.
#' @inheritParams eval_quantile_from_network
#' @noRd
snap_to_atoms <- function(distribution, atoms_obj, p, lo, hi, value) {
  n <- length(p)
  # The candidate atom bracketing `value` on each side; NA where the support has
  # no atom on that side.
  below <- rep(NA_real_, n)
  above <- rep(NA_real_, n)
  for (k in seq_len(n)) {
    lo_atom <- discretes::prev_discrete(
      atoms_obj, value[k], include_from = TRUE
    )
    hi_atom <- discretes::next_discrete(
      atoms_obj, value[k], include_from = TRUE
    )
    if (length(lo_atom) > 0L) below[k] <- lo_atom
    if (length(hi_atom) > 0L) above[k] <- hi_atom
  }
  uniq <- unique(c(below, above))
  uniq <- uniq[!is.na(uniq)]
  if (length(uniq) == 0L) {
    return(value)
  }
  f_a <- eval_cdf(distribution, uniq)
  pmf_a <- eval_pmf(distribution, uniq)
  f_lower <- f_a - pmf_a
  # Test each side's candidate against the exact jump condition and snap to
  # whichever atom's jump contains `p` (at most one can).
  for (cand in list(above, below)) {
    j <- match(cand, uniq)
    in_jump <- !is.na(cand) & (f_lower[j] < p) & (p <= f_a[j])
    in_jump[is.na(in_jump)] <- FALSE
    value[in_jump] <- cand[in_jump]
  }
  value
}

#' Legacy Quantile Algorithm (no structured support)
#'
#' The per-probability bisection used for distributions defined with a legacy
#' `.vtype` string and no structured support. Without a support the locations of
#' any atoms are unknown, so this path is restricted to continuous distributions
#' (as it has always been). Calls `encapsulate_p()` to bracket the solutions and
#' `directional_inverse()` to run the bisection.
#'
#' @inheritParams eval_quantile_from_network
#' @returns The `at`-quantiles, a numeric vector the same length as `at`.
#' @noRd
quantile_legacy <- function(distribution, at, tol, maxiter) {
  if (vtype(distribution) != "continuous") {
    stop(
      "The current quantile algorithm can suffer from low precision with ",
      "non-continuous distributions that lack a structured support, so this ",
      "functionality is disabled for now."
    )
  }
  n <- length(at)
  ord <- order(at)
  at <- at[ord]
  x <- at
  i_na <- which(is.na(at))
  i_zero <- which(at == 0)
  n_zero <- length(i_zero)
  i_positive <- which(at > 0 & at <= 1)
  i_other <- setdiff(seq_len(n), c(i_na, i_zero, i_positive))
  x[i_other] <- NaN
  if (n_zero > 0) {
    r <- encapsulate_p(distribution, p = 0, direction = "right")
    if (is.infinite(r[1L])) {
      x[i_zero] <- r[1L]
    } else {
      x[i_zero] <- directional_inverse(
        distribution,
        p = 0, low = r[1L], high = r[2L], tol = tol,
        maxiter = maxiter, direction = "right"
      )
    }
  }
  r <- encapsulate_p(distribution, p = at[i_positive], direction = "left")
  low <- rep(r[1L], n)
  for (i in i_positive) {
    p <- at[i]
    if (isTRUE(p == at[i - 1L])) {
      x[i] <- low[i + 1L] <- x[i - 1L]
    } else {
      x[i] <- low[i + 1L] <- directional_inverse(
        distribution,
        p = p, low = low[i], high = r[2L], tol = tol,
        maxiter = maxiter, direction = "left"
      )
    }
  }
  x[ord] <- x
  x
}

#' Find a range of possible outcomes
#'
#' In order to run the directional inverse algorithm, we need to know
#' approximately where the solution lies. This function
#' finds a range of possible outcomes where the cdf evaluates to values
#' (probabilities) contain the vector `p`, and therefore should come
#' before the inversion algorithm begins.
#'
#' @param p Vector of values between 0 and 1 (inclusive).
#' @param direction One of `"left"` for calculating left-inverse, or
#' `"right"` for calculating right-inverse.
#' @note If 0 or 1 are included in the vector `p`, one of the endpoints might
#' be infinite.
#' @returns A range of values containing the solutions to the left
#' inverse of the CDF at `p`.
#' @noRd
#' @inheritParams eval_quantile_from_network
encapsulate_p <- function(distribution, p, direction) {
  if (length(p) == 0) {
    return(c(NA, NA))
  }
  p_min <- min(p)
  p_max <- max(p)
  if (direction == "left") {
    cdf_gt <- `>=`
    cdf_lt <- `<`
    survival_gt <- `>`
  } else if (direction == "right") {
    cdf_gt <- `>`
    cdf_lt <- `<=`
    survival_gt <- `>=`
  } else {
    stop(
      "`direction` must be one of 'left' or 'right'. Received '",
      direction, "'."
    )
  }
  left <- -1
  right <- 1
  cdf_p <- eval_cdf(distribution, at = p)
  cdf_left <- eval_cdf(distribution, at = left)
  while (cdf_gt(cdf_left, p_min)) {
    left <- 2 * left
    cdf_left <- eval_cdf(distribution, at = left)
  }
  if (p_max >= 0.9 && !is.null(distribution$survival)) {
    survival_right <- eval_survival(distribution, at = right)
    while (survival_gt(survival_right, 1 - p_max)) {
      right <- 2 * right
      survival_right <- eval_survival(distribution, at = right)
    }
  } else {
    cdf_right <- eval_cdf(distribution, at = right)
    while (cdf_lt(cdf_right, p_max)) {
      right <- 2 * right
      cdf_right <- eval_cdf(distribution, at = right)
    }
  }
  if (p_min > 0 && is.infinite(left)) {
    left <- -.Machine$double.xmax
  }
  if (p_max < 1 && is.infinite(right)) {
    right <- .Machine$double.xmax
  }
  c(left, right)
}


#' Algorithm to Compute a Directional Inverse
#'
#' Calculates the smallest value for which a function `f`
#' evaluates to be greater than or equal to `y` -- that is,
#' the left inverse of `f` at `y`.
#' @param p Single value for which to calculate the left inverse.
#' @param low,high Single numeric values forming a range
#' within which to search for the solution.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number
#' of iterations
#' @details This algorithm works by progressively
#' cutting the specified range in half, moving into the left or right
#' half depending on where the solution is.
#' @returns The left inverse of the CDF evaluated at `p`.
#' @noRd
#' @inheritParams encapsulate_p
directional_inverse <- function(distribution, p, low, high, tol, maxiter,
                                direction) {
  stopifnot(low <= high)
  if (is.na(p)) {
    return(p)
  }
  if (direction == "left") {
    ineq <- `<=`
  } else if (direction == "right") {
    ineq <- `<`
  } else {
    stop(
      "`direction` must be one of 'left' or 'right'. Received '",
      direction, "'."
    )
  }
  max_tol <- tol
  w <- .Machine$double.xmax
  i <- 0L
  slope <- 1
  mid <- (high + low) / 2
  while (w > tol && i <= maxiter) {
    i <- i + 1L
    cdf_low <- eval_cdf(distribution, at = low)
    cdf_mid <- eval_cdf(distribution, at = mid)
    cdf_high <- eval_cdf(distribution, at = high)
    slope_left <- (cdf_mid - cdf_low) / w * 2
    slope_right <- (cdf_high - cdf_mid) / w * 2
    slope <- max(slope, min(slope_left, slope_right, na.rm = TRUE),
      na.rm = TRUE
    )
    tol <- min(max_tol / slope, tol, na.rm = TRUE)
    if (ineq(p, cdf_mid)) {
      high <- mid
    } else {
      low <- mid
    }
    if (low == high) {
      return(low)
    }
    w <- high - low
    mid <- (high + low) / 2
  }
  if (i == maxiter && w > tol) {
    warning(
      "Maximum number of iterations reached before ",
      "tolerance was achieved."
    )
  }
  mid
}
