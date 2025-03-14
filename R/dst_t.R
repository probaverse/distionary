#' Student t Distribution
#'
#' Makes a Student t distribution.
#'
#' @param df Degrees of freedom; single positive numeric.
#' @returns A Student t distribution.
#' @examples
#' dst_t(3)
#'
#' @export
dst_t <- function(df) {
  checkmate::assert_numeric(df, 0, len = 1)
  if (is.na(df)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(df = df),
    density = \(x) stats::dt(x, df = df),
    cdf = \(x) stats::pt(x, df = df),
    quantile = \(p) stats::qt(p, df = df),
    realise = \(n) stats::rt(n, df = df),
    survival = \(x) stats::pt(x, df = df, lower.tail = FALSE),
    mean = ifelse(df > 1, 0, NaN),
    median = 0,
    variance = {
      if (df > 2) {
        df / (df - 2)
      } else if (df > 1) {
        Inf
      } else {
        NaN
      }
    },
    skewness = ifelse(df > 3, 0, NaN),
    kurtosis_exc = {
      if (df > 4) {
        6 / (df - 4)
      } else if (df > 2) {
        Inf
      } else {
        NaN
      }
    },
    range = c(-Inf, Inf),
    .name = "Student t",
    .vtype = "continuous",
  )
}
