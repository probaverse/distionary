#' Chi-Squared Distribution
#'
#' Makes a Chi-Squared distribution.
#'
#' @param df degrees of freedom parameter; single positive numeric.
#' @returns A Chi-Squared distribution
#' @examples
#' dst_chisq(3)
#'
#' @export
dst_chisq <- function(df) {
  checkmate::assert_numeric(df, 0, len = 1)
  if (is.na(df)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(df = df),
    density = \(x) stats::dchisq(x, df = df),
    cdf = \(x) stats::pchisq(x, df = df),
    quantile = \(p) stats::qchisq(p, df = df),
    realise = \(n) stats::rchisq(n, df = df),
    survival = \(x) stats::pchisq(x, df = df, lower.tail = FALSE),
    mean = df,
    median = df * (1 - 2 / 9 * df)^3,
    variance = 2 * df,
    skewness = sqrt(8 / df),
    kurtosis_exc = 12 / df,
    range = c(0, Inf),
    .name = "Chi-Squared",
    .vtype = "continuous"
  )
}
