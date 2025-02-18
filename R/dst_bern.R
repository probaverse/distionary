#' Bernoulli Distribution
#'
#' Makes a Bernoulli distribution, representing the outcome of a
#' single trial with a given success probability.
#'
#' @param prob Probability of success.
#' @returns A Bernoulli distribution.
#' @examples
#' dst_bern(0.3)
#' @export
dst_bern <- function(prob) {
  if (prob < 0 || prob > 1) {
    stop("prob must be within 0 and 1.")
  }
  d <- dst_binom(size = 1, prob = prob)
  parameters(d)$size <- NULL
  attr(d, "name") <- "Bernoulli"
  d
}
