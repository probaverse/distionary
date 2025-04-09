#' Weibull Distribution
#'
#' Makes a Weibull distribution.
#'
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single positive numeric.
#' @returns A Weibull distribution.
#' @examples
#' dst_weibull(1, 1)
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
dst_weibull <- function(shape, scale) {
  checkmate::assert_numeric(shape, 0, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  if (is.na(shape) || is.na(scale)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(shape = shape, scale = scale),
    density = \(x) stats::dweibull(x, shape = shape, scale = scale),
    cdf = \(x) stats::pweibull(x, shape = shape, scale = scale),
    quantile = \(p) stats::qweibull(p, shape = shape, scale = scale),
    realise = \(n) stats::rweibull(n, shape = shape, scale = scale),
    survival = \(x) stats::pweibull(
      x,
      shape = shape, scale = scale, lower.tail = FALSE
    ),
    mean = scale * gamma(1 + 1 / shape),
    variance = scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2),
    skewness = {
      g1 <- gamma(1 + 1 / shape)
      g2 <- gamma(1 + 2 / shape)
      g3 <- gamma(1 + 3 / shape)
      (2 * g1^3 - 3 * g1 * g2 + g3) / (g2 - g1^2)^(3 / 2)
    },
    kurtosis_exc = {
      g1 <- gamma(1 + 1 / shape)
      g2 <- gamma(1 + 2 / shape)
      g3 <- gamma(1 + 3 / shape)
      g4 <- gamma(1 + 4 / shape)
      (-6 * g1^4 + 12 * g1^2 * g2 - 3 * g2^2 - 4 * g1 * g3 + g4) /
        (g2 - g1^2)^2
    },
    range = c(0, Inf),
    .name = "Weibull",
    .vtype = "continuous"
  )
}
