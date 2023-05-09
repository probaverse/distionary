#' Dimension of a Distribution
#'
#' Retrieves the number of variables associated with a probability distribution.
#'
#' @inheritParams eval_cdf
#' @return Integer.
#' @export
#' @examples
#' dimension(dst_norm(0, 1))
dimension <- function(distribution) {
  attr(distribution, "dimension")
}
