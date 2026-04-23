#' Exponential Distribution
#'
#' Makes an Exponential distribution.
#'
#' @param rate Rate parameter; single positive numeric.
#' @returns An Exponential distribution.
#' @examples
#' dst_exp(1)
#' @export
dst_exp <- function(rate) {
  checkmate::assert_numeric(rate, 0, len = 1)
  if (is.na(rate)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(rate = rate),
    density = function(x) {
      stats::dexp(x, rate = rate)
    },
    cdf = function(x) {
      stats::pexp(x, rate = rate)
    },
    quantile = function(p) {
      stats::qexp(p, rate = rate)
    },
    realise = function(n) {
      stats::rexp(n, rate = rate)
    },
    survival = function(x) {
      stats::pexp(x, rate = rate, lower.tail = FALSE)
    },
    mean = 1 / rate,
    median = log(2) / rate,
    variance = 1 / rate^2,
    skewness = 2,
    kurtosis_exc = 6,
    range = c(0, Inf),
    .name = "Exponential",
    .vtype = "continuous"
  )
}
