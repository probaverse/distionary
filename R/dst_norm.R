#' Normal (Gaussian) Distribution
#'
#' Makes a Normal (Gaussian) distribution.
#'
#' @param mean,sd Mean and standard deviation of the distribution.
#' @returns A Normal distribution.
#' @examples
#' dst_norm(0, 1)
#' @export
dst_norm <- function(mean, sd) {
  if (sd == 0) {
    return(dst_degenerate(mean))
  }
  if (sd < 0) {
    stop("'sd' parameter must be non-negative.")
  }
  distribution(
    parameters = list(mean = mean, sd = sd),
    density = \(x) stats::dnorm(x, mean = mean, sd = sd),
    cdf = \(x) stats::pnorm(x, mean = mean, sd = sd),
    quantile = \(p) stats::qnorm(p, mean = mean, sd = sd),
    realise = \(n) stats::rnorm(n, mean = mean, sd = sd),
    survival = \(x) stats::pnorm(
      x,
      mean = mean, sd = sd, lower.tail = FALSE
    ),
    mean = mean,
    median = mean,
    variance = sd^2,
    stdev = sd,
    skewness = 0,
    kurtosis_exc = 0,
    range = c(-Inf, Inf),
    .name = "Normal",
    .vtype = "continuous"
  )
}
