#' Gamma Distribution
#'
#' Makes a distribution belonging to the family of
#' Gamma distributions.
#'
#' @param shape Shape parameter; positive.
#' @param rate Rate parameter; positive.
#' @returns A distribution.
#' @examples
#' dst_gamma(2, 1)
#' @export
dst_gamma <- function(shape, rate) {
  if (shape <= 0) {
    stop("shape paramter must be positive.")
  }
  if (rate <= 0) {
    stop("rate parameter must be positive.")
  }
  dst_parametric(
    "gamma", shape = shape, rate = rate,
    .variable = "continuous", .env = "package:stats"
  )
}



