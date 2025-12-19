#' Student t Distribution
#'
#' Makes a Student t distribution.
#'
#' @param df Degrees of freedom; single positive numeric.
#' @returns A Student t distribution.
#' @examples
#' dst_t(3)
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
dst_t <- function(df) {
  checkmate::assert_numeric(df, 0, len = 1)
  if (is.na(df)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(df = df),
    density = function(x) {
      stats::dt(x, df = df)
    },
    cdf = function(x) {
      stats::pt(x, df = df)
    },
    quantile = function(p) {
      stats::qt(p, df = df)
    },
    realise = function(n) {
      stats::rt(n, df = df)
    },
    survival = function(x) {
      stats::pt(x, df = df, lower.tail = FALSE)
    },
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
