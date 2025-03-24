#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric.
#' @return Object of class "dst" of a GPD.
#' @examples
#' dst_gpd(1, 1)
#' @export
dst_gpd <- function(scale, shape) {
	if (scale <= 0) {
		stop("'scale' parameter must be positive.")
	}
  dst_parametric(
    "gpd", location = 0, scale = scale, shape = shape,
    .variable = "continuous", .env = "package:distionary"
  )
}
