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
#'
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
    parameters = list(m = m, n = n, k = k),
    pmf = \(x) stats::dhyper(x, m = m, n = n, k = k),
    cdf = \(x) stats::phyper(x, m = m, n = n, k = k),
    quantile = \(p) stats::qhyper(p, m = m, n = n, k = k),
    realise = \(n) stats::rhyper(n, m = m, n = n, k = k),
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
