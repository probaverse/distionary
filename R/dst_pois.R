#' Poisson Distribution
#'
#' Makes a Poisson distribution.
#' @param lambda Mean of the Poisson distribution; single positive numeric.
#' @returns A Poisson distribution.
#' @examples
#' dst_pois(1)
#' @export
dst_pois <- function(lambda) {
  checkmate::assert_numeric(lambda, 0, len = 1)
  if (is.na(lambda)) {
    return(dst_null())
  }
  distribution(
    parameters = list(lambda = lambda),
    pmf = \(x) stats::dpois(x, lambda = lambda),
    cdf = \(x) stats::ppois(x, lambda = lambda),
    quantile = \(p) stats::qpois(p, lambda = lambda),
    realise = \(n) stats::rpois(n, lambda = lambda),
    survival = \(x) stats::ppois(x, lambda = lambda, lower.tail = FALSE),
    mean = lambda,
    variance = lambda,
    skewness = lambda^(-0.5),
    kurtosis_exc = 1 / lambda,
    range = c(0, Inf),
    .name = "Poisson",
    .vtype = "discrete"
  )
}
