#' Generalized Extreme Value Distribution
#'
#' Makes a distribution belonging to the family of
#' Generalized Extreme Value (GEV) distributions.
#'
#' @param location Location parameter; numeric.
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric.
#' @examples
#' dst_gev(0, 1, 1)
#' @export
dst_gev <- function(location, scale, shape) {
  if (scale <= 0) {
    stop("'scale' parameter must be positive.")
  }
  distribution(
    parameters = list(location = location, scale = scale, shape = shape),
    cdf = \(x) pgev(
      x, location = location, scale = scale, shape = shape
    ),
    quantile = \(p) qgev(p, location = location, scale = scale, shape = shape),
    density = \(x) dgev(p, location = location, scale = scale, shape = shape),
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
    skewness = {
      if (shape == 0) {
        12 * sqrt(6) * zeta(3) / pi^3
      } else if (shape < 1 / 3) {
        g1 <- gamma(1 - shape)
        g2 <- gamma(1 - 2 * shape)
        g3 <- gamma(1 - 3 * shape)
        sign(shape) * (g3 - 3 * g2 * g1 + 2 * g1^3) / (g2 - g1^2)^(3 / 2)
      } else {
        NaN
      }
    },
    kurtosis_exc = {
      if (shape == 0) {
        12 / 5
      } else if (shape < 1 / 4) {
        g1 <- gamma(1 - shape)
        g2 <- gamma(1 - 2 * shape)
        g3 <- gamma(1 - 3 * shape)
        g4 <- gamma(1 - 4 * shape)
        (g4 - 4 * g4 * g1 - 3 * g2^2 + 12 * g2 * g1^2 - 6 * g1^4) /
          (g2 - g1^2)^2
      } else {
        NaN
      }
    },
    range = gev_range(location, scale, shape),
    .name = "Generalised Extreme Value",
    .vtype = "continuous"
  )
}
