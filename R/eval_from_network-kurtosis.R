#' @noRd
eval_kurtosis_exc_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  kurtosis(distribution) - 3
}

#' @noRd
eval_kurtosis_from_network <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")
  if (is_intrinsic(distribution, "kurtosis_exc")) {
    kurtosis_exc(distribution) + 3
  } else {
    algorithm_kurtosis(distribution, ...)
  }
}

#' @noRd
algorithm_kurtosis <- function(distribution, tol = 1e-7, ...) {
  mu <- mean(distribution)
  if (is.nan(mu) || is.infinite(mu)) {
    return(NaN)
  }
  sigma <- stdev(distribution)
  if (is.nan(sigma) || is.infinite(sigma) || sigma == 0) {
    return(NaN)
  }
  expect_over_support(
    distribution, function(x) ((x - mu) / sigma)^4, tol = tol, ...
  )
}
