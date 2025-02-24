#' Degenerate Distribution
#'
#' A degenerate distribution assigns a 100% probability to one outcome.
#' @param location Outcome of the distribution; single positive numeric.
#' @returns A degenerate distribution
#' @examples
#' d <- dst_degenerate(5)
#' realise(d)
#' variance(d)
#' @export
dst_degenerate <- function(location) {
  checkmate::assert_numeric(location, len = 1)
  if (is.na(location)) {
    return(dst_null())
  }
  distribution(
    parameters = list(location = location),
    cdf = \(x) as.numeric(x >= location),
    quantile = function(p) {
      res <- rep(location, length(p))
      res[p < 0 | p > 1] <- NaN
      res[is.na(p)] <- NA_real_
      res
    },
    pmf = \(x) as.numeric(x == location),
    realise = \(n) rep(location, n),
    survival = \(x) as.numeric(x < location),
    mean = location,
    stdev = 0,
    variance = 0,
    skewness = NaN,
    kurtosis_exc = NaN,
    .name = "Degenerate",
    .vtype = "discrete"
  )
}
