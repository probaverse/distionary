#' Negative binomial Distribution
#'
#' Makes a Negative Binomial distribution, corresponding to the number
#' of failures in a sequence of independent trials until a given number
#' of successes are observed.
#'
#' @param prob Probability of a successful trial; single numeric
#' between 0 and 1.
#' @param size Number of successful trials; single positive numeric.
#' @returns A Negative Binomial distribution.
#' @examples
#' d <- dst_nbinom(10, 0.5)
#'
#' # This version of the Negative Binomial distribution does not count
#' # the successes.
#' range(d)
#' @export
dst_nbinom <- function(size, prob) {
  checkmate::assert_numeric(size, 0, len = 1)
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(size) || is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(size = size, prob = prob),
    pmf = \(x) stats::dnbinom(x, size = size, prob = prob),
    cdf = \(x) stats::pnbinom(x, size = size, prob = prob),
    quantile = \(p) stats::qnbinom(p, size = size, prob = prob),
    realise = \(n) stats::rnbinom(n, size = size, prob = prob),
    survival = \(x) stats::pnbinom(
      x,
      size = size, prob = prob, lower.tail = FALSE
    ),
    mean = (1 - prob) * size / prob,
    variance = (1 - prob) * size / prob^2,
    skewness = (2 - prob) / sqrt((1 - prob) * size),
    kurtosis_exc = 6 / size + prob^2 / ((1 - prob) * size),
    range = c(0, Inf),
    .name = "Negative Binomial",
    .vtype = "discrete"
  )
}
