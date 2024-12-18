#' Chi-squared Distribution
#'
#' Makes a distribution belonging to the family of
#' Chi-squared distributions.
#'
#' @param df degrees of freedom
#' @returns A distribution.
#' @examples
#' dst_chisq(3)
#'
#' @export
dst_chisq <- function(df) {
  if (df < 0) {
    stop('df must be non-negative')
  }
  dst_parametric(
    "chisq", df = df, .variable = "continuous", .env = "package:stats"
  )
}
