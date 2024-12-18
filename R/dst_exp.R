#' Exponential Distribution
#'
#' Makes a distribution belonging to the family of
#' exponential distributions.
#'
#' @param rate Rate.
#' @returns A distribution.
#' @examples
#' dst_exp(1)
#'
#' @export
dst_exp <- function(rate) {
  if (rate <= 0) {
    stop('rate must be greater than 0.')
  }
  dst_parametric(
    "exp", rate = rate, .variable = "continuous", .env = "package:stats"
  )
}



