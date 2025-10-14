#' Generalised Pareto Distribution
#'
#' Makes a Generalized Pareto (GP) distribution, corresponding to the
#' limiting distribution of excesses over a threshold.
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single positive numeric.
#' This is also the extreme value index, so that `shape > 0` is heavy
#' tailed, and `shape < 0` is short-tailed.
#' @return A Generalised Pareto Distribution.
#' @examples
#' # Short-tailed example
#' short <- dst_gp(1, -1)
#' range(short)
#' mean(short)
#'
#' # Heavy-tailed example
#' heavy <- dst_gp(1, 1)
#' range(heavy)
#' mean(heavy)
#'
#' # Light-tailed example (a Gumbel distribution)
#' light <- dst_gp(1, 0)
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
dst_gp <- function(scale, shape) {
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, len = 1)
  if (is.na(scale) || is.na(shape)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(scale = scale, shape = shape),
    cdf = \(x) pgp(x, scale = scale, shape = shape),
    survival = \(x) pgp(
      x,
      scale = scale, shape = shape, lower.tail = FALSE
    ),
    quantile = \(p) qgp(p, scale = scale, shape = shape),
    density = \(x) dgp(x, scale = scale, shape = shape),
    mean = ifelse(shape < 1, scale / (1 - shape), Inf),
    variance = ifelse(
      shape < 1 / 2,
      scale^2 / (1 - shape)^2 / (1 - 2 * shape),
      Inf
    ),
    skewness = ifelse(
      shape < 1 / 3,
      2 * (1 + shape) * sqrt(1 - 2 * shape) /
        (1 - 3 * shape),
      Inf
    ),
    kurtosis_exc = ifelse(
      shape < 1 / 4,
      3 * (1 - 2 * shape) * (2 * shape^2 + shape + 3) /
        ((1 - 3 * shape) * (1 - 4 * shape)) - 3,
      Inf
    ),
    range = c(0, gp_upper(scale = scale, shape = shape)),
    .name = "Generalised Pareto",
    .vtype = "continuous"
  )
}
