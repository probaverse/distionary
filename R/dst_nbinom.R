#' Negative binomial Distribution
#'
#' Makes a Negative Binomial distribution, corresponding to the number
#' of failures in a sequence of independent trials until a given number
#' of successes are observed.
#'
#' @param prob Probability of a successful trial; single numeric
#' between 0 and 1.
#' @param size Number of successful trials; single positive numeric.
#' @returns A Negative Binomial distribution.
#' @examples
#' d <- dst_nbinom(10, 0.5)
#'
#' # This version of the Negative Binomial distribution does not count
#' # the successes.
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
dst_nbinom <- function(size, prob) {
  checkmate::assert_numeric(size, 0, len = 1)
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(size) || is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(size = size, prob = prob),
    pmf = \(x) stats::dnbinom(x, size = size, prob = prob),
    cdf = \(x) stats::pnbinom(x, size = size, prob = prob),
    quantile = \(p) stats::qnbinom(p, size = size, prob = prob),
    realise = \(n) stats::rnbinom(n, size = size, prob = prob),
    survival = \(x) stats::pnbinom(
      x,
      size = size, prob = prob, lower.tail = FALSE
    ),
    mean = (1 - prob) * size / prob,
    variance = (1 - prob) * size / prob^2,
    skewness = (2 - prob) / sqrt((1 - prob) * size),
    kurtosis_exc = 6 / size + prob^2 / ((1 - prob) * size),
    range = c(0, Inf),
    .name = "Negative Binomial",
    .vtype = "discrete"
  )
}
