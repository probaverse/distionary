#' Hypergeometric Distribution
#'
#' Creates a Hypergeometric distribution, representing the
#' number of red balls drawn from an urn containing multiple colours,
#' using a scoop that holds a fixed number of balls.
#'
#' @param m The number of red balls in the urn; single positive integer.
#' @param n The number of non-red balls in the urn; single positive integer.
#' @param k the number of balls drawn from the urn (between 0 and `m + n`);
#' single positive integer.
#' @returns A Hypergeometric distribution.
#' @examples
#' dst_hyper(15, 50, 10)
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
dst_hyper <- function(m, n, k) {
  checkmate::assert_numeric(m, 0, len = 1)
  checkmate::assert_numeric(n, 0, len = 1)
  if (is.na(m) || is.na(n)) {
    return(dst_null())
  }
  N <- m + n
  checkmate::assert_numeric(k, 0, N, len = 1)
  if (is.na(k)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(m = m, n = n, k = k),
    pmf = \(x) stats::dhyper(x, m = m, n = n, k = k),
    cdf = \(x) stats::phyper(x, m = m, n = n, k = k),
    quantile = \(p) stats::qhyper(p, m = m, n = n, k = k),
    realise = \(nn) stats::rhyper(nn, m = m, n = n, k = k),
    survival = \(x) stats::phyper(
      x,
      m = m, n = n, k = k, lower.tail = FALSE
    ),
    mean = k * m / N,
    variance = k * (m / N) * (n / N) * ((N - k) / (N - 1)),
    skewness = (N - 2 * m) * sqrt(N - 1) * (N - 2 * k) /
      (sqrt(k * m * n * (N - k)) * (N - 2)),
    kurtosis_exc = (
      (N - 1) * N^2 *
        (N * (N + 1) - 6 * m * n - 6 * k * (N - k)) +
        6 * k * m * n * (N - k) * (5 * N - 6)
    ) / (
      k * m * n * (N - k) * (N - 2) * (N - 3)
    ),
    range = c(max(0, k - n), min(m, k)),
    .name = "Hypergeometric",
    .vtype = "discrete"
  )
}
