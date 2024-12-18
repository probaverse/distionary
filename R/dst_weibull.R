#' Weibull Distribution
#'
#' Makes a distribution belonging to the family of
#' Weibull distributions.
#'
#' @param scale Scale parameter; positive.
#' @param shape Shape parameter; positive.
#' @returns A distribution.
#' @examples
#' dst_weibull(1, 1)
#'
#' @export
dst_weibull <- function(shape, scale) {
  if (scale <= 0) {
    stop('scale parameter must be positive.')
  }
  if (shape <= 0) {
    stop('shape parameter must be positive.')
  }
  dst_parametric(
    "weibull", shape = shape, scale = scale,
    .variable = "continuous", .env = "package:stats"
  )
}
