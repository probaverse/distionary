#' Convenience Functions for Bivariate Distributions
#'
#' Evaluate bivariate distributions using `x` and `y` arguments, instead
#' of providing a list in the `at` argument of `eval_<representation>()`.
#'
#' @param x,y Numeric vectors specifying where to evaluate the bivariate
#' representation at.
#' @inheritParams eval_cdf
#' @export
eval_bi_cdf <- function(distribution, x, y, ...) {
  eval_cdf(distribution, at = list(x, y), ...)
}
