#' @param .l A list of vectors, one for each dimension of the distribution,
#' to evaluate the distributional representation at. Optionally, a single
#' vector of the same length, for evaluating the representation at a single
#' point.
#' @rdname cdf
#' @export
eval_multi_cdf <- function(distribution, .l) UseMethod("eval_multi_cdf")

#' @export
eval_cdf.default <- function(distribution, .l) {
  stop("Can't find a cdf for this distribution.")
}
