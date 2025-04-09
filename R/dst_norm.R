#' Normal (Gaussian) Distribution
#'
#' Makes a Normal (Gaussian) distribution.
#'
#' @param mean Mean of the distribution. Single numeric.
#' @param sd Standard deviation of the distribution.
#' Single positive numeric.
#' @returns A Normal distribution.
#' @examples
#' dst_norm(0, 1)
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
dst_norm <- function(mean, sd) {
  checkmate::assert_numeric(mean, len = 1)
  checkmate::assert_numeric(sd, 0, len = 1)
  if (is.na(mean) || is.na(sd)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(mean = mean, sd = sd),
    density = \(x) stats::dnorm(x, mean = mean, sd = sd),
    cdf = \(x) stats::pnorm(x, mean = mean, sd = sd),
    quantile = \(p) stats::qnorm(p, mean = mean, sd = sd),
    realise = \(n) stats::rnorm(n, mean = mean, sd = sd),
    survival = \(x) stats::pnorm(
      x,
      mean = mean, sd = sd, lower.tail = FALSE
    ),
    mean = mean,
    median = mean,
    variance = sd^2,
    stdev = sd,
    skewness = 0,
    kurtosis_exc = 0,
    range = c(-Inf, Inf),
    .name = "Normal",
    .vtype = "continuous"
  )
}
