#' Cauchy Distribution
#'
#' Makes a Cauchy distribution.
#'
#' @param location Location parameter; single numeric.
#' @param scale Scale parameter; single positive numeric.
#' @returns A Cauchy distribution.
#' @examples
#' d <- dst_cauchy(0, 1)
#'
#' # Moments do not exist for the Cauchy distribution.
#' mean(d)
#' variance(d)
#' @export
dst_cauchy <- function(location, scale) {
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  if (is.na(location) || is.na(scale)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(location = location, scale = scale),
    density = \(x) stats::dcauchy(x, location = location, scale = scale),
    cdf = \(x) stats::pcauchy(x, location = location, scale = scale),
    quantile = \(p) stats::qcauchy(p, location = location, scale = scale),
    realise = \(n) stats::rcauchy(n, location = location, scale = scale),
    survival = \(x) stats::pcauchy(
      x,
      location = location, scale = scale, lower.tail = FALSE
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
