#' Geometric Distribution
#'
#' Makes a Geometric distribution, corresponding to the number of failures
#' in a sequence of independent trials before observing a success.
#'
#' @param prob Probability of success in each trial; single numeric
#' between 0 and 1.
#' @returns A Geometric distribution.
#' @examples
#' d <- dst_geom(0.4)
#'
#' # This version of the Geometric distribution does not count the success.
#' range(d)
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
dst_geom <- function(prob) {
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(prob = prob),
    pmf = function(x) {
      stats::dgeom(x, prob = prob)
    },
    cdf = function(x) {
      stats::pgeom(x, prob = prob)
    },
    quantile = function(p) {
      stats::qgeom(p, prob = prob)
    },
    realise = function(n) {
      stats::rgeom(n, prob = prob)
    },
    survival = function(x) {
      stats::pgeom(x, prob = prob, lower.tail = FALSE)
    },
    mean = (1 - prob) / prob,
    variance = (1 - prob) / prob^2,
    skewness = ifelse(prob < 1, (2 - prob) / sqrt(1 - prob), NaN),
    kurtosis_exc = ifelse(prob < 1, 6 + prob^2 / (1 - prob), NaN),
    range = c(0, Inf),
    .name = "Geometric",
    .vtype = "discrete"
  )
}
