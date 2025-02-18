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
  if (is.na(size) || is.na(prob)) {
    return(dst_null())
  }
  if (length(size) != 1 || length(prob) != 1) {
    stop("Input parameters must have length 1.")
  }
  if (size < 0) {
    stop("Size must be non-negative")
  }
  if (prob < 0 || prob > 1) {
    stop("prob must be within 0 and 1")
  }
  distribution(
    parameters = list(size = size, prob = prob),
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
