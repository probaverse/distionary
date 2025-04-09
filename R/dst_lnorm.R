#' Log Normal Distribution
#'
#' Makes a Log Normal distribution, which is the distribution of
#' the exponential of a Normally distributed random variable.
#' @param meanlog Mean of the log of the random variable;
#' single numeric.
#' @param sdlog Standard deviation of the log of the random variable;
#' single positive numeric.
#' @returns A Log Normal distribution.
#' @examples
#' dst_lnorm(0, 1)
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
dst_lnorm <- function(meanlog, sdlog) {
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  if (is.na(meanlog) || is.na(sdlog)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(meanlog = meanlog, sdlog = sdlog),
    density = \(x) stats::dlnorm(x, meanlog = meanlog, sdlog = sdlog),
    cdf = \(x) stats::plnorm(x, meanlog = meanlog, sdlog = sdlog),
    quantile = \(p) stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog),
    realise = \(n) stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog),
    survival = \(x) stats::plnorm(
      x,
      meanlog = meanlog, sdlog = sdlog, lower.tail = FALSE
    ),
    mean = exp(meanlog + sdlog^2 / 2),
    median = exp(meanlog),
    variance = {
      ev <- exp(sdlog^2)
      (ev - 1) * ev * exp(2 * meanlog)
    },
    skewness = {
      ev <- exp(sdlog^2)
      (ev + 2) * sqrt(ev - 1)
    },
    kurtosis_exc = {
      e4 <- exp(4 * sdlog^2)
      e3 <- exp(3 * sdlog^2)
      e2 <- exp(2 * sdlog^2)
      e4 + 2 * e3 + 3 * e2 - 6
    },
    range = c(0, Inf),
    .name = "Log Normal",
    .vtype = "continuous"
  )
}
