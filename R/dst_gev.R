#' Generalised Extreme Value Distribution
#'
#' Makes a Generalised Extreme Value (GEV) distribution, which is the
#' limiting distribution of the maximum.
#'
#' @param location Location parameter; single numeric.
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single numeric.
#' This is also the extreme value index,
#' so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.
#' @returns A GEV distribution.
#' @examples
#' # Short-tailed example
#' short <- dst_gev(0, 1, -1)
#' range(short)
#' mean(short)
#'
#' # Heavy-tailed example
#' heavy <- dst_gev(0, 1, 1)
#' range(heavy)
#' mean(heavy)
#'
#' # Light-tailed example (a Gumbel distribution)
#' light <- dst_gev(0, 1, 0)
#' range(light)
#' mean(light)
#' @export
dst_gev <- function(location, scale, shape) {
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, len = 1)
  if (is.na(location) || is.na(scale) || is.na(shape)) {
    return(dst_null())
  }
  distribution(
    parameters = list(location = location, scale = scale, shape = shape),
    cdf = \(x) pgev(
      x,
      location = location, scale = scale, shape = shape
    ),
    quantile = \(p) qgev(p, location = location, scale = scale, shape = shape),
    density = \(x) dgev(x, location = location, scale = scale, shape = shape),
    mean = {
      if (shape >= 1) {
        Inf
      } else if (shape == 0) {
        location - scale * digamma(1)
      } else {
        location + (scale * (gamma(1 - shape) - 1)) / shape
      }
    },
    variance = {
      if (shape > 0.5) {
        Inf
      } else if (shape == 0) {
        scale^2 * pi^2 / 6
      } else {
        scale^2 * (gamma(1 - 2 * shape) - gamma(1 - shape)^2) / shape^2
      }
    },
    range = c(
      gev_lower(location, scale, shape),
      gev_upper(location, scale, shape)
    ),
    .name = "Generalised Extreme Value",
    .vtype = "continuous"
  )
}
