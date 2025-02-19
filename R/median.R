#' Median of a Distribution
#'
#' Finds the median of a distribution.
#'
#' @importFrom stats median
#'
#' @param x Distribution to calculate median from.
#' @param ... Not used.
#' @returns Median of a distribution; single numeric.
#' @details
#' Median is calculated as the 0.5-quantile when not found in the
#' distribution. So, when the median is
#' non-unique, the smallest of the possibilities is taken.
#' @examples
#' d <- dst_gamma(3, 3)
#' median(d)
#' @export
median.dst <- function(x, ...) {
  ellipsis::check_dots_empty()
  eval_property(x, "median")
}
