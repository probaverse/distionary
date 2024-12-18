#' @importFrom stats median
#'
#' @details
#' Median is calculated as the 0.5-quantile. So, when the median is
#' non-unique, we take the smallest of the possibilities.
#' @rdname moments
#' @export
median.dst <- function(x, ...) {
  eval_quantile(x, at = 0.5)
}
