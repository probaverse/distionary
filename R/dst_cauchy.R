#' Cauchy Distribution
#'
#' Makes a distribution belonging to the family of
#' cauchy distributions.
#'
#' @param location Location parameter
#' @param scale positive
#'
#' @examples
#' dst_cauchy(0, 1)
#'
#'@export
dst_cauchy <- function(location, scale){
  if (scale <= 0){
    stop('Scale must be positive')
  }
  distribution(
    parameters = list(location = location, scale = scale),
    density = \(x) stats::dcauchy(x, location = location, scale = scale),
    cdf = \(x) stats::pcauchy(x, location = location, scale = scale),
    quantile = \(p) stats::qcauchy(p, location = location, scale = scale),
    realise = \(n) stats::rcauchy(n, location = location, scale = scale),
    survival = \(x) stats::pcauchy(
      x, location = location, scale = scale, lower.tail = FALSE
    ),
    mean = NaN,
    median = location,
    variance = NaN,
    skewness = NaN,
    kurtosis_exc = NaN,
    range = c(-Inf, Inf),
    .vtype = "continuous",
    .name = "Cauchy"
  )
}
