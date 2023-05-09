#' Cumulative Distribution Function
#'
#' Evaluate a cumulative distribution function (cdf).
#'
#' @param distribution A distribution, inheriting the class
#' `"dst"`, `"bidst"`, or `"multidst"`.
#' @param at A vector of values to evaluate the univariate distributional
#' representation at.
#' @param x,y Vectors of values to evaluate the bivariate distributional
#' representation at.
#' @param .l A list of vectors, one for each dimension of the distribution,
#' to evaluate the distributional representation at. Optionally, a single
#' vector of the same length, for evaluating the representation at a single
#' point.
#' @return The evaluated cdf in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @family distributional representations
#' @order 1
#' @examples
#' d1 <- dst_unif(0, 4)
#' d2 <- dst_pois(1.1)
#' eval_cdf(d1, at = 0:4)
#' enframe_cdf(d1, at = 0:4)
#' enframe_cdf(d1, d2, at = 0:4)
#' @rdname eval_cdf
#' @export
eval_cdf <- function(distribution, at) UseMethod("eval_cdf")

#' @export
eval_cdf.dst <- function(distribution, at) {
  stop("Can't find a cdf for this distribution.")
}

#' @export
eval_cdf.bidst <- function(distribution, at) {
  stop("Expecting a univariate distribution; received bivariate.")
}

#' @export
eval_cdf.multidst <- function(distribution, at) {
  d <- dimension(distribution)
  if (d == 1) {
    return(eval_multi_cdf(distribution, .l = list(at)))
  }
  stop("Expecting a univariate distribution; received ", d, "-variate.")
}
