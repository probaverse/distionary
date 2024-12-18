#' The Student t Distribution
#'
#' Makes a distribution belonging to the family of
#' t distributions.
#'
#' @param df Degrees of freedom; positive.
#' @returns A distribution.
#' @examples
#' dst_t(3)
#'
#'@export
dst_t <- function(df){
  if (df <= 0) {
    stop('df must be positive.')
  }
  dst_parametric("t", df = df, .variable = "continuous", .env = "package:stats")
}
