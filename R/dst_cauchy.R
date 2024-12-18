#' Cauchy Distribution
#'
#' Makes a distribution belonging to the family of
#' cauchy distributions.
#'
#' @param location Location parameter
#' @param scale positive
#' @returns A distribution.
#' @examples
#' dst_cauchy(0, 1)
#'
#'@export
dst_cauchy <- function(location, scale){
  if (scale <= 0){
    stop('Scale must be positive')
  }
  dst_parametric(
    "cauchy", location = location, scale = scale,
    .variable = "continuous", .env = "package:stats"
  )
}
