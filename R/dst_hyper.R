#' Hypergeometric Distribution
#'
#' Creates a Hypergeometric distribution, representing the
#' number of red balls drawn from an urn containing multiple colours,
#' using a scoop that holds a fixed number of balls.
#'
#' @param m The number of red balls in the urn.
#' @param n The number of non-red balls in the urn.
#' @param k the number of balls drawn from the urn (between 0 and `m + n`).
#' @returns A Hypergeometric distribution.
#' @examples
#' dst_hyper(15, 50, 10)
#'
#' @export
dst_hyper <- function(m, n, k) {
  if (is.na(m) || is.na(n) || is.na(k)) {
    return(dst_null())
  }
  if (length(m) != 1 || length(n) != 1 || length(k) != 1) {
    stop("Input parameters must have length 1.")
  }
  if (m < 0) {
    stop("m must be non-negative")
  }
  if (n < 0) {
    stop("n must be non-negative")
  }
  if (k < 0 || k > m + n) {
    stop("k must be between 0 and m+n.")
  }
  N <- m + n
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
