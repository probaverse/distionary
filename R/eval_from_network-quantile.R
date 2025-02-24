#' Evaluate Quantiles from a CDF
#'
#' Pulls together all the pieces needed to calculate the left inverse
#' of the CDF. Intended for internal use only. Calls the
#' `encapsulate_p()` function to figure out where to start looking for
#' solutions, then `directional_inverse()` to run the algorithm.
#'
#' @param distribution A distribution having access to a cdf.
#' @param at A vector of values for which to evaluate the quantile function.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number
#' of iterations (at least 1); length 1 vectors.
#' @returns The `at`-quantiles of the distribution. Numeric vector
#' of values between 0 and 1.
#' @noRd
eval_quantile_from_network <- function(
    distribution, at, tol = 1e-9, maxiter = 200) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at, 0, 1)
  checkmate::assert_numeric(tol, 0, len = 1)
  checkmate::assert_integerish(maxiter, 1, len = 1)
  n <- length(at)
  if (n == 0) {
    return(numeric(0L))
  }
  ord <- order(at)
  at <- at[ord]
  x <- at
  i_na <- which(is.na(at))
  i_zero <- which(at == 0)
  n_zero <- length(i_zero)
  i_positive <- which(at > 0 & at <= 1)
  n_positive <- length(i_positive)
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
  high <- r[2L]
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
  # cat("w:", w, "tol:", tol, "i:", i, "slope:", slope, "\n")
  if (i == maxiter && w > tol) {
    warning(
      "Maximum number of iterations reached before ",
      "tolerance was achieved."
    )
  }
  mid
}
