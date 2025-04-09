#' F Distribution
#'
#' Makes an F distribution.
#'
#' @param df1,df2 Degrees of freedom of the numerator and denominator,
#' both single positive numerics.
#' @returns An F distribution.
#' @examples
#' dst_f(2, 3)
#' @srrstats {G2.0} Assertions on lengths of inputs (asserting that
#' inputs expected to be single- or multi-valued) are explicitly
#' tested for distribution parameters; implicitly through evaluation
#' functions.
#' @srrstats {G2.0a} Explicit secondary documentation of expectations on
#' lengths of inputs have been provided where relevant. See `dst_norm()`
#' for an example.
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.1a} Explicit secondary documentation of expectations on
#' data types of all vector inputs are provided. See `dst_norm()` for an
#' example.
#' @srrstats {G2.2} Prohibiting or restricting submission of multivariate
#' input (i.e., distributions) to univariate parameters is
#' done using the checkmate package for relevant functions (e.g., `dst_*()`
#' specifications)
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.6} distionary asserts one-dimensional input where required
#' (e.g., `dst_*()` specifications) using the checkmate package.
#' @srrstats {G2.13} Checks for missing data are conducted for distribution
#' parameters and a Null distribution is made to handle missing data.
#' See `dst_norm()` for an example. Checks are made for built-in
#' representations, but the onus is on the user for self-defined
#' distributions.
#' @srrstats {G2.15} Functions never assume non-missingness, and never
#' pass arguments to another function with `na.rm = FALSE`-type parameters.
#' This is most relevant for functions like `dst_norm()`.
#' @export
dst_f <- function(df1, df2) {
  checkmate::assert_numeric(df1, 0, len = 1)
  checkmate::assert_numeric(df2, 0, len = 1)
  if (is.na(df1) || is.na(df2)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(df1 = df1, df2 = df2),
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
