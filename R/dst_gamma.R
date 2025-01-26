#' Gamma Distribution
#'
#' Makes a distribution belonging to the family of
#' Gamma distributions.
#'
#' @param shape Shape parameter; positive.
#' @param rate Rate parameter; positive.
#'
#' @examples
#' dst_gamma(2, 1)
#' @export
dst_gamma <- function(shape, rate) {
  if (shape <= 0) {
    stop("shape paramter must be positive.")
  }
  if (rate <= 0) {
    stop("rate parameter must be positive.")
  }
  distribution(
    parameters = list(shape = shape, rate = rate),
    density = \(x) stats::dgamma(x, shape = shape, rate = rate),
    cdf = \(x) stats::pgamma(x, shape = shape, rate = rate),
    quantile = \(p) stats::qgamma(p, shape = shape, rate = rate),
    realise = \(n) stats::rgamma(n, shape = shape, rate = rate),
    survival = \(x) stats::pgamma(
      x, shape = shape, rate = rate, lower.tail = FALSE
    ),
    mean = shape / rate,
    variance = shape / rate^2,
    skewness = 2 / sqrt(shape),
    kurtosis_exc = 6 / shape,
    range = c(0, Inf),
    .name = "Gamma",
    .vtype = "continuous"
  )
}



