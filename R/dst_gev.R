#' Generalized Extreme Value Distribution
#'
#' Makes a distribution belonging to the family of
#' Generalized Extreme Value (GEV) distributions.
#'
#' @param location Location parameter; numeric.
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric.
#' @returns A distribution.
#' @examples
#' dst_gev(0, 1, 1)
#' @export
dst_gev <- function(location, scale, shape) {
  if (scale <= 0) {
    stop("'scale' parameter must be positive.")
  }
  dst_parametric(
    "gev", location = location, scale = scale, shape = shape,
    .variable = "continuous", .env = "package:distionary"
  )
}
