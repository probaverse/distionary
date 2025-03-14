#' Pearson Type III distribution
#'
#' Makes a Pearson Type III distribution, which is a Gamma distribution,
#' but shifted.
#'
#' @param location Location parameter, specifying how to shift the
#' Gamma distribution; single numeric.
#' @param scale Scale parameter of the Gamma distribution;
#' single positive numeric.
#' @param shape Shape parameter of the Gamma distribution;
#' single positive numeric.
#' @returns A Pearson Type III distribution.
#' @srrstats {PD3.0} *Manipulation of probability distributions should
#' very generally be analytic, with numeric manipulations only
#' implemented with clear justification (ideally including references).*
#' This only applies to the Pearson Type III and Log Pearson Type III
#' that manipulates the Gamma distribution from the stats package.
#' Manipulations are analytic.
#' @examples
#' dst_pearson3(1, 1, 1)
#' @export
dst_pearson3 <- function(location, scale, shape) {
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, 0, len = 1)
  if (is.na(location) || is.na(scale) || is.na(shape)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(
      location = location, scale = scale, shape = shape
    ),
    cdf = \(x) stats::pgamma(x - location, scale = scale, shape = shape),
    survival = \(x) stats::pgamma(
      x - location,
      scale = scale, shape = shape, lower.tail = FALSE
    ),
    density = \(x) stats::dgamma(
      x - location,
      scale = scale, shape = shape
    ),
    quantile = \(p) location + stats::qgamma(p, shape = shape, scale = scale),
    realise = \(n) location + stats::rgamma(n, shape = shape, scale = scale),
    mean = location + scale * shape,
    variance = shape * scale^2,
    skewness = 2 / sqrt(shape),
    kurtosis_exc = 6 / shape,
    .name = "Pearson Type III",
    .vtype = "continuous"
  )
}
