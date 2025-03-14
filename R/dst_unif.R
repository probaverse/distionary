#' Uniform Distribution
#'
#' Makes a Uniform distribution.
#'
#' @param min,max Minimum and maximum of the distribution.
#' Single numerics.
#' @returns A Uniform distribution.
#' @examples
#' dst_unif(0, 1)
#' @export
dst_unif <- function(min, max) {
  checkmate::assert_numeric(min, len = 1)
  if (is.na(min)) {
    return(dst_null())
  }
  checkmate::assert_numeric(max, min, len = 1)
  if (is.na(max)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(min = min, max = max),
    density = \(x) stats::dunif(x, min = min, max = max),
    cdf = \(x) stats::punif(x, min = min, max = max),
    quantile = \(p) stats::qunif(p, min = min, max = max),
    realise = \(n) stats::runif(n, min = min, max = max),
    survival = \(x) stats::punif(x, min = min, max = max, lower.tail = FALSE),
    mean = (min + max) / 2,
    median = (min + max) / 2,
    variance = (min - max)^2 / 12,
    skewness = 0,
    kurtosis_exc = -6 / 5,
    range = c(min, max),
    .name = "Uniform",
    .vtype = "continuous"
  )
}
