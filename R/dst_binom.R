#' Binomial Distribution
#'
#' Makes a distribution belonging to the family of
#' binomial distributions.
#'
#' @param size Number of trials.
#' @param prob Success probability for each trial.
#' @returns A distribution.
#' @examples
#' dst_binom(10, 0.6)
#' @export
dst_binom <- function(size, prob) {
  if (size < 0) {
    stop("Size must be non-negative")
  }
  if (prob < 0 || prob > 1) {
    stop("prob must be within 0 and 1")
  }
  dst_parametric(
    "binom", size = size, prob = prob,
    .variable = "discrete", .env = "package:stats"
  )
}
