#' Weibull Distribution
#'
#' Makes a Weibull distribution.
#'
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single positive numeric.
#' @returns A Weibull distribution.
#' @examples
#' dst_weibull(1, 1)
#'
#' @export
dst_weibull <- function(shape, scale) {
  checkmate::assert_numeric(shape, 0, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  if (is.na(shape) || is.na(scale)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(shape = shape, scale = scale),
    density = \(x) stats::dweibull(x, shape = shape, scale = scale),
    cdf = \(x) stats::pweibull(x, shape = shape, scale = scale),
    quantile = \(p) stats::qweibull(p, shape = shape, scale = scale),
    realise = \(n) stats::rweibull(n, shape = shape, scale = scale),
    survival = \(x) stats::pweibull(
      x,
      shape = shape, scale = scale, lower.tail = FALSE
    ),
    mean = scale * gamma(1 + 1 / shape),
    variance = scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2),
    skewness = {
      g1 <- gamma(1 + 1 / shape)
      g2 <- gamma(1 + 2 / shape)
      g3 <- gamma(1 + 3 / shape)
      (2 * g1^3 - 3 * g1 * g2 + g3) / (g2 - g1^2)^(3 / 2)
    },
    kurtosis_exc = {
      g1 <- gamma(1 + 1 / shape)
      g2 <- gamma(1 + 2 / shape)
      g3 <- gamma(1 + 3 / shape)
      g4 <- gamma(1 + 4 / shape)
      (-6 * g1^4 + 12 * g1^2 * g2 - 3 * g2^2 - 4 * g1 * g3 + g4) /
        (g2 - g1^2)^2
    },
    range = c(0, Inf),
    .name = "Weibull",
    .vtype = "continuous"
  )
}
