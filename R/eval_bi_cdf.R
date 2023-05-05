#' @param x,y Vectors of values to evaluate the (bivariate) distributional
#' representation at.
#' @rdname cdf
#' @export
eval_bi_cdf <- function(distribution, x, y) UseMethod("eval_bi_cdf")

#' @rdname cdf
#' @export
eval_bi_cdf.default <- function(distribution, x, y) {
  xy <- vctrs::vec_recycle_common(x, y)
  eval_multi_cdf(distribution, list(xy[[1]], xy[[2]]))
}
