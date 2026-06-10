#' @noRd
eval_variance_from_network <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")
  if (is_intrinsic(distribution, "stdev")) {
    stdev(distribution)^2
  } else {
    algorithm_variance(distribution, ...)
  }
}

#' @noRd
algorithm_variance <- function(distribution, tol = 1e-9, ...) {
  mu <- mean(distribution)
  if (is.nan(mu) || is.infinite(mu)) {
    return(NaN)
  }
  expect_over_support(distribution, function(x) (x - mu)^2, tol = tol, ...)
}
