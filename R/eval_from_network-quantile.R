#' Invert a CDF
#'
#' Solves for the inverse of a cumulative distribution function at a vector of
#' probabilities. This is a numerical routine and nothing more: it takes the
#' function to invert and the facts it needs about the shape of the answer, and
#' it does not reach into a distribution object.
#'
#' A single bisection advances every requested probability at once, so one
#' (vectorized) call to `cdf` serves the whole vector per iteration rather than
#' one call per probability. Where atoms are supplied, a probability landing
#' inside an atom's jump is returned as that atom *exactly*, which bisection
#' alone cannot do.
#'
#' @param cdf The function to invert. Must be vectorized.
#' @param at Probabilities at which to invert, each in `[0, 1]`.
#' @param hull Length-2 numeric `c(lower, upper)`: the outermost points of the
#' set the solution lives in --- for a distribution, the two ends of its
#' support. Either endpoint may be infinite, and both usually are for an
#' unbounded distribution.
#'
#' It has two jobs, and only one of them is forgiving. For interior
#' probabilities it is merely where the bracket starts: the search widens or
#' narrows from there as needed, so a hull that is too wide, or too narrow,
#' still gives the right answer. For `at == 0` and `at == 1` it *is* the
#' answer, returned as given. A hull that does not match the true support will
#' therefore return quietly wrong boundary values while every interior value
#' stays correct, which is a nasty way to be wrong --- so it should be the
#' support's own ends, not an approximation of them.
#' @param ... Not used; must be empty. Present so that the arguments below are
#' matched by name.
#' @param atoms The points carrying positive probability, as a `discretes`
#' object, or `NULL` if there are none. When supplied, `pmf` is required.
#' @param pmf The probability mass function, vectorized. Needed only to size
#' the atoms' jumps.
#' @param side Which inverse to take: `"left"` (the usual quantile function) is
#' the smallest `x` with `cdf(x) >= p`; `"right"` is the smallest `x` with
#' `cdf(x) > p`.
#'
#' The two differ only where the CDF is **flat at level `p` over a stretch of
#' positive length** --- where `p` is a value the CDF takes and then holds. The
#' left inverse gives the start of that stretch, the right inverse its end.
#'
#' A jump is *not* such a case, which is worth being explicit about. A
#' probability landing strictly inside an atom's jump is a level the CDF skips
#' over entirely, so there is no stretch to choose an end of, and both inverses
#' return that atom. The two can part company only at `p = cdf(a)` exactly, the
#' top of the jump, and then only if a flat stretch follows the atom: a gap
#' before the next mass, as between the atoms of a Poisson, where the left
#' inverse gives `a` and the right gives the next atom. Where density resumes
#' immediately after the atom, as in a mixed distribution, there is again no
#' stretch and both inverses give `a`.

#'
#' **`side` does not apply at `at == 0` and `at == 1`,** where the answer is the
#' corresponding end of `hull` either way. This is not the two inverses
#' agreeing; a convention overrides both, because each is degenerate at one end:
#' the left inverse of 0 is `-Inf` for every distribution (a CDF is everywhere
#' at least 0) and the right inverse of 1 is `Inf` for every distribution (a CDF
#' never exceeds 1). Taking the right inverse at 0 and the left inverse at 1 is
#' what puts the boundary answers on the support.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number of
#' iterations (at least 1); length 1 vectors.
#' @returns The inverse at each element of `at`, a numeric vector the same
#' length as `at`. `NA` in gives `NA` out.
#' @noRd
invert_cdf <- function(cdf,
                       at,
                       hull,
                       ...,
                       atoms = NULL,
                       pmf = NULL,
                       side = c("left", "right"),
                       tol = 1e-9,
                       maxiter = 200) {
  rlang::check_dots_empty()
  checkmate::assert_function(cdf)
  checkmate::assert_numeric(at, 0, 1)
  checkmate::assert_numeric(hull, len = 2)
  side <- rlang::arg_match(side)
  checkmate::assert_numeric(tol, 0, len = 1)
  checkmate::assert_integerish(maxiter, lower = 1, len = 1)
  n <- length(at)
  if (n == 0) {
    return(numeric(0L))
  }
  out <- rep(NaN, n)
  # Preserve NA (and NaN) inputs as-is.
  out[is.na(at)] <- at[is.na(at)]
  ok <- !is.na(at)
  # The boundaries come straight off the hull; see `side` above for why they
  # ignore it. `eval_quantile()` normally settles these before calling, so this
  # is here to keep the routine correct at any entry point, not for speed.
  out[ok & at == 0] <- hull[[1L]]
  out[ok & at == 1] <- hull[[2L]]
  is_interior <- ok & at > 0 & at < 1
  if (!any(is_interior)) {
    return(out)
  }
  p <- at[is_interior]
  br <- quantile_bracket(cdf, hull, p, side = side)
  sol <- bisect_quantile(
    cdf, p,
    lo = br[["lo"]], hi = br[["hi"]], side = side, tol = tol, maxiter = maxiter
  )
  value <- sol[["value"]]
  if (!is.null(atoms) && discretes::num_discretes(atoms) > 0) {
    if (is.null(pmf)) {
      stop("`pmf` is required when `atoms` are supplied.")
    }
    value <- snap_to_atoms(
      cdf, pmf, atoms, p, value = value, side = side, tol = tol
    )
  }
  out[is_interior] <- value
  out
}

#' Evaluate Quantiles from a CDF
#'
#' The network's quantile entry point: pulls the CDF, the support hull, and the
#' atoms off the distribution and hands them to `invert_cdf()`, which does the
#' work. Every distribution carries a support, so there is only one algorithm.
#'
#' @param distribution A distribution having access to a cdf.
#' @param at A vector of probabilities at which to evaluate the quantile.
#' @param side Passed to `invert_cdf()`. [eval_quantile()] always takes the
#' left inverse; `side` is not yet exposed there.
#' @param tol,maxiter Passed to `invert_cdf()`.
#' @returns The `at`-quantiles of the distribution. Numeric vector the same
#' length as `at`.
#' @noRd
eval_quantile_from_network <- function(distribution,
                                       at,
                                       side = c("left", "right"),
                                       tol = 1e-9,
                                       maxiter = 200) {
  checkmate::assert_class(distribution, "dst")
  side <- rlang::arg_match(side)
  s <- support(distribution)
  if (is.null(s)) {
    # Unreachable in practice: `distribution()` requires a support, and the one
    # distribution without one (Null) supplies its own quantile function.
    stop(
      "Deriving quantiles requires the distribution's support. ",
      "Specify `.support` when building the distribution."
    )
  }
  invert_cdf(
    cdf = function(x) eval_cdf(distribution, at = x),
    at = at,
    hull = support_hull(s),
    atoms = s[["atoms"]],
    pmf = function(x) eval_pmf(distribution, at = x),
    side = side,
    tol = tol,
    maxiter = maxiter
  )
}

#' Bracket the Interior Quantiles
#'
#' Finds a single `[lo, hi]` interval guaranteed to contain every interior
#' solution, so the bisection invariant holds for all probabilities at once.
#' Finite hull endpoints are used as-is; infinite ones are replaced by an
#' outward search that doubles away until the CDF clears the probability range.
#'
#' @param cdf The function being inverted.
#' @param hull Length-2 numeric `c(lower, upper)`.
#' @param p Vector of interior probabilities (strictly between 0 and 1).
#' @param side Which inverse is being taken.
#' @returns A list with entries `lo` and `hi`, each recycled to `length(p)`.
#' @noRd
quantile_bracket <- function(cdf, hull, p, side) {
  p_min <- min(p)
  p_max <- max(p)
  left <- if (is.finite(hull[[1L]])) hull[[1L]] else -1
  # Move below the support until `cdf(left) < p_min`. For a finite hull with an
  # atom at the lower endpoint, `cdf(lower)` can exceed `p_min`, so we still
  # step down (no mass lives below the support, so the CDF drops to 0 there).
  while (cdf(left) >= p_min) {
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
  right <- if (is.finite(hull[[2L]])) hull[[2L]] else 1
  # The left inverse needs `cdf(right) >= p_max`; the right inverse needs the
  # strict `cdf(right) > p_max`, so it may step one notch further out.
  below_max <- if (side == "left") `<` else `<=`
  while (below_max(cdf(right), p_max)) {
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

#' Vectorized Bisection
#'
#' Solves the inverse for an entire vector of probabilities at once. The
#' bracket invariant is maintained per element --- `cdf(lo) < p <= cdf(hi)` for
#' the left inverse, `cdf(lo) <= p < cdf(hi)` for the right --- and each
#' iteration evaluates `cdf` a single (vectorized) time, at the midpoints,
#' moving the appropriate endpoint of every element together.
#'
#' Convergence is on the *x-width* `hi - lo` falling below a combined
#' absolute/relative tolerance `tol * max(1, |lo|, |hi|)`, which targets a fixed
#' precision in the answer itself rather than in the probability, and behaves
#' well across tiny to very large magnitudes.
#'
#' Elements over a flat part of the CDF (a gap, or the plateau above an atom)
#' shrink their x-width normally; an exact atom is then recovered by the
#' trailing snap. The midpoint guard (`mid > lo & mid < hi`) is a floating-point
#' floor so the loop always terminates even if `tol` is below the representable
#' spacing.
#'
#' @param cdf The function being inverted.
#' @param p Vector of probabilities, each strictly between 0 and 1.
#' @param lo,hi Numeric vectors (same length as `p`) bracketing the solutions.
#' @param side Which inverse is being taken.
#' @param tol,maxiter Tolerance and iteration cap.
#' @returns A list with `value` (the midpoint estimate), `lo`, and `hi`, each a
#' numeric vector the same length as `p`.
#' @noRd
bisect_quantile <- function(cdf, p, lo, hi, side, tol, maxiter) {
  for (i in seq_len(maxiter)) {
    mid <- (lo + hi) / 2
    x_tol <- tol * pmax(1, abs(lo), abs(hi))
    active <- (hi - lo) > x_tol & mid > lo & mid < hi
    if (!any(active)) {
      break
    }
    f_mid <- cdf(mid)
    # Only this comparison differs between the two inverses.
    go_left <- if (side == "left") p <= f_mid else p < f_mid
    move_hi <- active & go_left
    move_lo <- active & !go_left
    hi[move_hi] <- mid[move_hi]
    lo[move_lo] <- mid[move_lo]
  }
  list(value = (lo + hi) / 2, lo = lo, hi = hi)
}

#' Snap Quantiles onto Atoms
#'
#' Where the answer for `p` is an atom `a`, the bisection only *approaches* `a`;
#' worse, common discrete CDFs (e.g. `ppois()`) fuzz their input by ~1e-7, so
#' the bracket can converge a hair below the true atom. This replaces such
#' estimates with the atom exactly. For each probability the two nearest atoms
#' (the largest `<= value` and the smallest `>= value`) are candidates, and one
#' is accepted precisely when `p` lies inside its jump. The jumps are disjoint
#' in `p`, so at most one candidate is ever accepted, and a `p` whose `value` is
#' genuinely between atoms is never snapped.
#'
#' @param cdf,pmf The functions being inverted and their mass function.
#' @param atoms_obj The atomic part of the support (a `discretes` object with at
#' least one atom).
#' @param p Vector of probabilities.
#' @param value Bisection estimates, returned unchanged where no atom is hit.
#' @param side Which inverse is being taken.
#' @param tol The bisection's tolerance, used to recognise an estimate that has
#' converged onto an atom.
#' @returns The `value` vector with atom-hitting entries replaced by the atoms.
#' @noRd
snap_to_atoms <- function(cdf, pmf, atoms_obj, p, value, side, tol) {
  n <- length(p)
  # The candidate atom bracketing `value` on each side; NA where the support
  # has no atom on that side.
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
  f_a <- cdf(uniq)
  pmf_a <- pmf(uniq)
  f_lower <- f_a - pmf_a
  # Test each side's candidate against the jump condition and snap to whichever
  # atom's jump contains `p` (at most one can).
  for (cand in list(above, below)) {
    j <- match(cand, uniq)
    # The atom `a` is the left inverse for `p` in `(F(a-), F(a)]`, and the
    # right inverse for `p` in `[F(a-), F(a))`.
    in_jump <- if (side == "left") {
      # Both comparisons are strict on the side that matters, so an exact `p`
      # falls out correctly without any tolerance.
      !is.na(cand) & (f_lower[j] < p) & (p <= f_a[j])
    } else {
      # The right inverse closes the *lower* end, and that is the end computed
      # by subtraction, so this is the one comparison a rounding error can
      # flip.
      lower_ok <- f_lower[j] < p | near_probability(f_lower[j], p)
      # At the top of the jump, `p == F(a)`, the atom is still the answer when
      # nothing flat follows it -- density resuming immediately, as in a mixed
      # distribution. When a gap follows instead, the answer is the next point
      # of increase, and the bisection will have converged there rather than
      # here, so requiring the estimate to have landed on `a` separates the two
      # without needing to know which case it is.
      converged_on_atom <- near_probability(p, f_a[j]) &
        abs(value - cand) <= tol * pmax(1, abs(value), abs(cand))
      converged_on_atom[is.na(converged_on_atom)] <- FALSE
      !is.na(cand) & lower_ok & (p < f_a[j] | converged_on_atom)
    }
    in_jump[is.na(in_jump)] <- FALSE
    value[in_jump] <- cand[in_jump]
  }
  value
}

#' Are Two Probabilities the Same Number?
#'
#' A tie test for the lower end of an atom's jump in `snap_to_atoms()`, used by
#' the right inverse only. `F(a) - pmf(a)` is `F(a-)` mathematically, but the
#' subtraction loses the last bits, so the two can disagree by a few units in
#' the last place --- enough to put a `p` sitting exactly on `F(a-)` on the
#' wrong side of a comparison the right inverse treats as closed.
#'
#' The tolerance is deliberately tight. It has to be looser than that rounding
#' error and tighter than any genuine gap between distinct probabilities, and
#' distributions with very many atoms put real jump endpoints close together.
#'
#' @param a,b Numeric vectors of probabilities.
#' @returns A logical vector.
#' @noRd
near_probability <- function(a, b) {
  !is.na(a) & !is.na(b) & abs(a - b) <= 1e-12 * pmax(1, abs(a), abs(b))
}
