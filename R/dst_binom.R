#' Binomial Distribution
#'
#' Makes a Binomial distribution, representing the number of successes
#' in a fixed number of independent trials.
#'
#' @param size Number of trials; single positive integer.
#' @param prob Success probability of each trial; single numeric
#' between 0 and 1.
#' @returns A binomial distribution.
#' @examples
#' dst_binom(10, 0.6)
#' @srrstats {G2.4a} Explicit conversion to `integer` via `as.integer()`
#' becomes superfluous after checking that a number is "integerish" using the
#' checkmate package.
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
dst_binom <- function(size, prob) {
  checkmate::assert_integerish(size, lower = 0, len = 1)
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(size) || is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(size = size, prob = prob),
    pmf = function(x) {
      stats::dbinom(x, size = size, prob = prob)
    },
    cdf = function(x) {
      stats::pbinom(x, size = size, prob = prob)
    },
    quantile = function(p) {
      stats::qbinom(p, size = size, prob = prob)
    },
    realise = function(n) {
      stats::rbinom(n, size = size, prob = prob)
    },
    survival = function(x) {
      stats::pbinom(x, size = size, prob = prob, lower.tail = FALSE)
    },
    mean = size * prob,
    variance = size * prob * (1 - prob),
    skewness = (1 - 2 * prob) / sqrt(size * prob * (1 - prob)),
    kurtosis_exc = (1 - 6 * prob * (1 - prob)) / (size * prob * (1 - prob)),
    range = c(0, size),
    .vtype = "discrete",
    .name = "Binomial"
  )
}
