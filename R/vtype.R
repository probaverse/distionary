#' Variable Type of a Distribution
#'
#' Retrieve the variable type of a distribution, such as
#' "continuous" or "discrete".
#'
#' @param distribution Distribution object.
#' @returns Single character with the variable type.
#' @examples
#' vtype(dst_beta(1, 2))
#' vtype(dst_bern(0.4))
#' vtype(distribution())
#' @export
vtype <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  attributes(distribution)[["vtype"]]
}
