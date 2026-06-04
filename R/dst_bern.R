#' Bernoulli Distribution
#'
#' Makes a Bernoulli distribution, representing the outcome of a
#' single trial with a given success probability.
#'
#' @param prob Probability of success; single numeric between 0 and 1.
#' @returns A Bernoulli distribution.
#' @examples
#' dst_bern(0.3)
#' @export
dst_bern <- function(prob) {
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(prob)) {
    return(dst_null())
  }
  d <- dst_binom(size = 1, prob = prob)
  parameters(d)$size <- NULL
  attr(d, "name") <- "Bernoulli"
  d
}
