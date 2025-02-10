#' @importFrom stats median
#'
#' @param x Distribution to calculate median from.
#' @param ... Not used.
#' @details
#' Median is calculated as the 0.5-quantile when not found in the
#' distribution. So, when the median is
#' non-unique, the smallest of the possibilities is taken.
#' @rdname moments
#' @export
median.dst <- function(x, ...) {
  ellipsis::check_dots_empty()
  eval_representation(x, "median")
}

eval_median_from_network <- function(distribution) {
  eval_quantile(distribution, at = 0.5)
}
