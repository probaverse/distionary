#' Negative binomial Distribution
#'
#' Makes a distribution belonging to the family of
#' negative binomial distributions.
#'
#' @param prob Probability parameter; between 0 and 1.
#' @param size Target for number of successful trials; non-negative.
#' @examples
#' dst_nbinom(10, 0.5)
#' @export
dst_nbinom <- function(size, prob) {
  if (prob < 0 || prob > 1) {
    stop('prob must be within 0 and 1.')
  }
  if (size <= 0) {
    stop('size must be positive.')
  }
  distribution(
    parameters = list(size = size, prob = prob),
    pmf = \(x) stats::dnbinom(x, size = size, prob = prob),
    cdf = \(x) stats::pnbinom(x, size = size, prob = prob),
    quantile = \(p) stats::qnbinom(p, size = size, prob = prob),
    realise = \(n) stats::rnbinom(n, size = size, prob = prob),
    survival = \(x) stats::pnbinom(
      x, size = size, prob = prob, lower.tail = FALSE
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
