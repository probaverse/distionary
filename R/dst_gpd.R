#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param location Location parameter; numeric.
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric.
#' @returns A distribution.
#' @examples
#' dst_gpd(0, 1, 1)
#' @export
dst_gpd <- function(location, scale, shape) {
	if (scale <= 0) {
		stop("'scale' parameter must be positive.")
	}
  dst_parametric(
    "gpd", location = location, scale = scale, shape = shape,
    .variable = "continuous", .env = "package:distionary"
  )
}
