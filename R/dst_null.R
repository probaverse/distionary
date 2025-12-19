#' Null Distribution
#'
#' Sometimes it's convenient to work with a distribution object that is
#' akin to a missing value. This is especially true when programmatically
#' outputting distributions, such as when a distribution fails to fit to
#' data. This function makes such a distribution object. It always evaluates
#' to `NA`.
#'
#' @returns A Null distribution.
#' @examples
#' x <- dst_null()
#' mean(x)
#' eval_pmf(x, at = 1:10)
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
dst_null <- function() {
  distribution(
    .parameters = NULL,
    cdf = function(x) {
      rep(NA_real_, length(x))
    },
    density = function(x) {
      rep(NA_real_, length(x))
    },
    hazard = function(x) {
      rep(NA_real_, length(x))
    },
    chf = function(x) {
      rep(NA_real_, length(x))
    },
    pmf = function(x) {
      rep(NA_real_, length(x))
    },
    odds = function(x) {
      rep(NA_real_, length(x))
    },
    return = function(x) {
      rep(NA_real_, length(x))
    },
    quantile = function(x) {
      rep(NA_real_, length(x))
    },
    mean = NA_real_,
    variance = NA_real_,
    skewness = NA_real_,
    kurtosis_exc = NA_real_,
    .name = "Null",
    .vtype = NA_character_
  )
}
