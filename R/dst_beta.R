#' Beta Distribution
#'
#' Makes a Beta distribution.
#' @param shape1,shape2 Shape parameters of the distribution;
#' single positive numerics.
#' @returns A Beta distribution.
#' @examples
#' dst_beta(2, 3)
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
dst_beta <- function(shape1, shape2) {
  checkmate::assert_numeric(shape1, 0, len = 1)
  checkmate::assert_numeric(shape2, 0, len = 1)
  if (is.na(shape1) || is.na(shape2)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(shape1 = shape1, shape2 = shape2),
    density = \(x) stats::dbeta(x, shape1, shape2),
    cdf = \(x) stats::pbeta(x, shape1, shape2),
    quantile = \(p) stats::qbeta(p, shape1, shape2),
    realise = \(n) stats::rbeta(n, shape1, shape2),
    survival = \(x) stats::pbeta(x, shape1, shape2, lower.tail = FALSE),
    mean = shape1 / (shape1 + shape2),
    variance = shape1 * shape2 / (shape1 + shape2)^2 /
      (shape1 + shape2 + 1),
    skewness = 2 * (shape2 - shape1) * sqrt(shape1 + shape2 + 1) /
      (shape1 + shape2 + 2) / sqrt(shape1 * shape2),
    range = c(0, 1),
    .vtype = "continuous",
    .name = "Beta"
  )
}
