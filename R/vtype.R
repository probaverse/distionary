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
#' vtype(distribution(
#'   cdf = pnorm,
#'   density = dnorm,
#'   .support = continuous()
#' ))
#' @export
vtype <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  attributes(distribution)[["vtype"]]
}

#' Variable type, for the property network.
#'
#' As with `eval_range_from_network()`: `vtype` is a derived property, so
#' `eval_property()` reaches it by computing rather than by finding it stored.
#'
#' @param distribution Distribution object.
#' @returns Single character.
#' @noRd
eval_vtype_from_network <- function(distribution) {
  vtype(distribution)
}
