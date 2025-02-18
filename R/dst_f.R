#' F Distribution
#'
#' Makes an F distribution.
#'
#' @param df1,df2 Degrees of freedom of the numerator and denominator,
#' both positive.
#' @returns An F distribution.
#' @examples
#' dst_f(2, 3)
#' @export
dst_f <- function(df1, df2) {
  if (df1 <= 0) {
    stop("df1 must be positive")
  }
  if (df2 <= 0) {
    stop("df2 must be positive")
  }
  distribution(
    parameters = list(df1 = df1, df2 = df2),
    density = \(x) stats::df(x, df1 = df1, df2 = df2),
    cdf = \(x) stats::pf(x, df1 = df1, df2 = df2),
    quantile = \(p) stats::qf(p, df1 = df1, df2 = df2),
    realise = \(n) stats::rf(n, df1 = df1, df2 = df2),
    survival = \(x) stats::pf(x, df1 = df1, df2 = df2, lower.tail = FALSE),
    mean = ifelse(df2 > 2, df2 / (df2 - 2), NaN),
    variance = ifelse(
      df2 > 4,
      2 * df2^2 * (df1 + df2 - 2) / (df1 * (df2 - 2)^2 * (df2 - 4)),
      NaN
    ),
    skewness = ifelse(
      df2 > 6,
      (2 * df1 + df2 - 2) * sqrt(8 * (df2 - 4)) /
        ((df2 - 6) * sqrt(df1 * (df1 + df2 - 2))),
      NaN
    ),
    kurtosis_exc = ifelse(
      df2 > 8,
      12 * (df1 * (5 * df2 - 22) * (df1 + df2 - 2) + (df2 - 4) * (df2 - 2)^2) /
        (df1 * (df2 - 6) * (df2 - 8) * (df1 + df2 - 2)),
      NaN
    ),
    range = c(0, Inf),
    .name = "F",
    .vtype = "continuous"
  )
}
