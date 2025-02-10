#' Pearson Type III distribution
#'
#' @param location,scale,shape Distribution parameters. Need scale > 0
#' and shape > 0.
#' @returns A Pearson Type III distribution.
#' @details This is the equivalent of the Gamma distribution in the
#' stats package with parameters `scale` and `shape`,
#' @export
dst_pearson3 <- function(location, scale, shape) {
  if (scale < 0) {
    stop("scale parameter cannot be less than 0.")
  }
  if (shape < 0) {
    stop("shape parameter cannot be less than 0.")
  }
  distribution(
    parameters = list(
      location = location, scale = scale, shape = shape
    ),
    cdf = \(x) stats::pgamma(x - location, scale = scale, shape = shape),
    survival = \(x) stats::pgamma(
      x - location, scale = scale, shape = shape, lower.tail = FALSE
    ),
    density = \(x) stats::dgamma(
      x - location, scale = scale, shape = shape
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
