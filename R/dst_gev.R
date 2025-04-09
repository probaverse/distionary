#' Generalised Extreme Value Distribution
#'
#' Makes a Generalised Extreme Value (GEV) distribution, which is the
#' limiting distribution of the maximum.
#'
#' @param location Location parameter; single numeric.
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single numeric.
#' This is also the extreme value index,
#' so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.
#' @returns A GEV distribution.
#' @examples
#' # Short-tailed example
#' short <- dst_gev(0, 1, -1)
#' range(short)
#' mean(short)
#'
#' # Heavy-tailed example
#' heavy <- dst_gev(0, 1, 1)
#' range(heavy)
#' mean(heavy)
#'
#' # Light-tailed example (a Gumbel distribution)
#' light <- dst_gev(0, 1, 0)
#' range(light)
#' mean(light)
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
dst_gev <- function(location, scale, shape) {
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, len = 1)
  if (is.na(location) || is.na(scale) || is.na(shape)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(location = location, scale = scale, shape = shape),
    cdf = \(x) pgev(
      x,
      location = location, scale = scale, shape = shape
    ),
    quantile = \(p) qgev(p, location = location, scale = scale, shape = shape),
    density = \(x) dgev(x, location = location, scale = scale, shape = shape),
    mean = {
      if (shape >= 1) {
        Inf
      } else if (shape == 0) {
        location - scale * digamma(1)
      } else {
        location + (scale * (gamma(1 - shape) - 1)) / shape
      }
    },
    variance = {
      if (shape > 0.5) {
        Inf
      } else if (shape == 0) {
        scale^2 * pi^2 / 6
      } else {
        scale^2 * (gamma(1 - 2 * shape) - gamma(1 - shape)^2) / shape^2
      }
    },
    range = c(
      gev_lower(location, scale, shape),
      gev_upper(location, scale, shape)
    ),
    .name = "Generalised Extreme Value",
    .vtype = "continuous"
  )
}
