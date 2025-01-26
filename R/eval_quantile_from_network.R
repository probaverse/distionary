#' Evaluate Quantiles from a CDF
#'
#' Intended for internal use only. Calls the `directional_inverse()`
#' algorithm for each new entry of `at`.
#'
#' @param distribution A distribution having access to a cdf.
#' @param at A vector of values for which to evaluate the quantile function.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number
#' of iterations
eval_quantile_from_network <- function(
    distribution, at, tol = 1e-9, maxiter = 200
) {
  n <- length(at)
  if (n == 0) return(numeric(0L))
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
        distribution, p = 0, low = r[1L], high = r[2L], tol = tol,
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
        distribution, p = p, low = low[i], high = r[2L], tol = tol,
        maxiter = maxiter, direction = "left"
      )
    }
  }
  x[ord] <- x
  x
}

#' Find a range of possible outcomes
#'
#' Find a range of possible outcomes where the cdf evaluates to values
#' that encapsulate the range of probabilities contained in the vector `p`.
#'
#' @param p Vector of values between 0 and 1 (inclusive).
#' @param direction One of `"left"` for calculating left-inverse, or
#' `"right"` for calculating right-inverse.
#' @note If 0 or 1 are included in the vector `p`, one of the endpoints might
#' be infinite.
#' @inheritParams eval_quantile_from_network
encapsulate_p <- function(distribution, p, direction) {
  if (length(p) == 0) return(c(NA, NA))
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
    stop("`direction` must be one of 'left' or 'right'. Received '",
         direction, "'.")
  }
  left <- -1
  right <- 1
  cdf_p <- eval_cdf(distribution, at = p)
  cdf_left <- eval_cdf(distribution, at = left)
  while (cdf_gt(cdf_left, p_min)) {
    left <- 2 * left
    cdf_left <- eval_cdf(distribution, at = left)
  }
  if (p_max >= 0.9) {
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
#' the left inverse of `f` at `y`. Intended for internal use only.

#' @param p Single value for which to calculate the left inverse.
#' @param low,high Single numeric values forming a range
#' within which to search for the solution.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number
#' of iterations
#' @details This algorithm works by progressively
#' cutting the specified range in half, so that the width
#' of the range after k iterations is 1/2^k times the
#' original width.
#' @export
#' @inheritParams encapsulate_p
directional_inverse <- function(distribution, p, low, high, tol, maxiter,
                                direction) {
  stopifnot(low <= high)
  if (is.na(p)) return(p)
  if (direction == "left") {
    ineq <- `<=`
  } else if (direction == "right") {
    ineq <- `<`
  } else {
    stop("`direction` must be one of 'left' or 'right'. Received '",
         direction, "'.")
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
                 na.rm = TRUE)
    tol <- min(max_tol / slope, tol, na.rm = TRUE)
    if (ineq(p, cdf_mid)) {
      high <- mid
      discrete <- prev_discrete(
        distribution, from = high, n = 1L, include_from = TRUE
      )
    } else {
      low <- mid
      discrete <- next_discrete(
        distribution, from = low, n = 1L, include_from = TRUE
      )
    }
    low_high <- narrow_by_discretes(
      distribution, p = p, low = low, high = high, discrete = discrete,
      direction = direction
    )
    low <- low_high[1L]
    high <- low_high[2L]
    if (low == high) return(low)
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

#' Narrow a range of possible values by a discrete
#'
#' When finding the inverse of the cdf at `p`, and with the solution
#' between `low` and `high`, this function narrows this range based
#' on an intermediate discrete value, and zeroes-in on that
#' discrete value if it's the solution.
#'
#' @param p Value of the cdf to calculate inverse at.
#' @param low,high Single numerics specifying the lower and upper bound
#' to look between.
#' @param discrete Numeric value indicating a discrete point to possibly
#' narrow the range by. Could be `NA`; could have length 0; could be
#' outside of the range `c(low, high)`.
#' @note The benefit of this step when inverting a cdf is to return
#' the exact discrete value if `p` falls within its jump discontinuity.
#' @inheritParams encapsulate_p
narrow_by_discretes <- function(distribution, p, low, high, discrete,
                                direction) {
  if (is.na(discrete) || !length(discrete) ||
      discrete < low || discrete > high) {
    return(c(low, high))
  }
  cdf_upper <- prob_left(distribution, of = discrete, inclusive = TRUE)
  cdf_lower <- prob_left(distribution, of = discrete, inclusive = FALSE)
  if (direction == "left") {
    lower_lt <- `<`
    upper_gt <- `>=`
  } else if (direction == "right") {
    lower_lt <- `<=`
    upper_gt <- `>`
  } else {
    stop("`direction` must be one of 'left' or 'right'. Received '",
         direction, "'.")
  }
  if (lower_lt(cdf_lower, p)) {
    low <- discrete
  }
  if (upper_gt(cdf_upper, p)) {
    high <- discrete
  }
  c(low, high)
}

