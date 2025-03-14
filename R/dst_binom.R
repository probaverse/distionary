#' Binomial Distribution
#'
#' Makes a Binomial distribution, representing the number of successes
#' in a fixed number of independent trials.
#'
#' @param size Number of trials; single positive integer.
#' @param prob Success probability of each trial; single numeric
#' between 0 and 1.
#' @returns A binomial distribution.
#' @examples
#' dst_binom(10, 0.6)
#' @export
dst_binom <- function(size, prob) {
  checkmate::assert_numeric(size, 0, len = 1)
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(size) || is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(size = size, prob = prob),
    pmf = \(x) stats::dbinom(x, size = size, prob = prob),
    cdf = \(x) stats::pbinom(x, size = size, prob = prob),
    quantile = \(p) stats::qbinom(p, size = size, prob = prob),
    realise = \(n) stats::rbinom(n, size = size, prob = prob),
    survival = \(x) stats::pbinom(
      x,
      size = size, prob = prob, lower.tail = FALSE
    ),
    mean = size * prob,
    variance = size * prob * (1 - prob),
    skewness = (1 - 2 * prob) / sqrt(size * prob * (1 - prob)),
    kurtosis_exc = (1 - 6 * prob * (1 - prob)) / (size * prob * (1 - prob)),
    range = c(0, size),
    .vtype = "discrete",
    .name = "Binomial"
  )
}
