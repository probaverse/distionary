#' Bernoulli Distribution
#'
#' Makes a distribution belonging to the family of
#' bernoulli distributions.
#'
#' @param prob Probability of success.
#' @returns A distribution.
#' @examples
#' dst_bern(0.3)
#'
#' @export
dst_bern <- function(prob){
  if (prob < 0 || prob > 1) {
    stop('prob must be within 0 and 1.')
  }
  dst_binom(size = 1, prob = prob)
}
