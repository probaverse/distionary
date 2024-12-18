#' Negative binomial Distribution
#'
#' Makes a distribution belonging to the family of
#' negative binomial distributions.
#'
#' @param prob Probability parameter; between 0 and 1.
#' @param size Target for number of successful trials; non-negative.
#' @returns A distribution.
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
  dst_parametric(
    "nbinom", size = size, prob = prob,
    .variable = "discrete", .env = "package:stats"
  )
}
