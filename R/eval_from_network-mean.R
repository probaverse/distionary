#' @noRd
eval_mean_from_network <- function(distribution, tol = 1e-9, ...) {
  checkmate::assert_class(distribution, "dst")
  expect_over_support(distribution, function(x) x, tol = tol, ...)
}
