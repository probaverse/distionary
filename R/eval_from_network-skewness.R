#' @noRd
eval_skewness_from_network <- function(distribution, tol = 1e-9, ...) {
  checkmate::assert_class(distribution, "dst")
  mu <- mean(distribution)
  if (is.nan(mu) || is.infinite(mu)) {
    return(NaN)
  }
  sigma <- stdev(distribution)
  if (is.nan(sigma) || is.infinite(sigma) || sigma == 0) {
    return(NaN)
  }
  expect_over_support(
    distribution, function(x) ((x - mu) / sigma)^3, tol = tol, ...
  )
}
