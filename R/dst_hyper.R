#' Hypergeometric Distribution
#'
#' Makes a distribution belonging to the family of
#' hypergeometric distributions.
#'
#' @param N the population size
#' @param K the number of success states in the population
#' @param n the number of draws
#' @returns A Hypergeometric distribution.
#' @examples
#' dst_hyper(1, 5, 10)
#'
#' @export
dst_hyper <- function(K, N, n) {
  if (N < 0){
    stop('N must be non-negative')
  }
  if (K < 0){
    stop('K must be non-negative')
  }
  if (n < 0){
    stop('n must be non-negative')
  }
  distribution(
    parameters = list(K = K, N = N, n = n),
    density = \(x) stats::dhyper(x, K = K, N = N, n = n),
    cdf = \(x) stats::phyper(x, K = K, N = N, n = n),
    quantile = \(p) stats::qhyper(p, K = K, N = N, n = n),
    realise = \(n) stats::rhyper(n, K = K, N = N, n = n),
    survival = \(x) stats::phyper(
      x, K = K, N = N, n = n, lower.tail = FALSE
    ),
    mean = (N - K) * K / N,
    variance = (N - K) * K / N * (N - K) / N * K / (N - 1),
    skewness = (N - 2 * K) * sqrt(N - 1) * (N - 2 * (N - K)) /
      (sqrt((N - K) * K * (N - K) * K) * (N - 2)),
    kurtosis_exc = (
      (N - 1) * N^2 *
        (N * (N + 1) - 6 * K * (N - K) - 6 * (N - K) * K) +
        6 * (N - K) * K * (N - K) * K * (5 * N - 6)
    ) / (
      (N - K) * K * (N - K) * K * (N - 2) * (N - 3)
    ),
    range = c(0, n),
    .name = "Hypergeometric",
    .vtype = "discrete"
  )
}
