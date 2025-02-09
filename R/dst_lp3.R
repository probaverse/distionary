#' Log Pearson Type III distribution
#'
#' @inheritParams dlpearson3
#' @returns A Log Pearson Type III distribution.
#' @export
dst_lp3 <- function(meanlog, sdlog, skew) {
  if (sdlog < 0) {
    stop("sdlog cannot be less than 0.")
  }
  # skewness = 2 / sqrt(shape), shape = 4 / skewness^2
  shape <- 4 / skew^2
  # sd = sqrt(shape) * scale
  scale <- sdlog / sqrt(shape)
  # mean = scale * shape
  shift <- mean - scale * shape
  cdf_gamma <-
  distribution(
    parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
    cdf = \(x) stats::pgamma(log(x) - shift, shape = shape, scale = scale),
    survival = \(x) stats::pgamma(
      log(x) - shift, shape = shape, scale = scale, lower.tail = FALSE
    ),
    density = \(x) stats::dgamma(
      log(x) - shift, shape = shape, scale = scale
    ) / x,
    quantile = \(p) exp(
      stats::qgamma(tau, shape = shape, scale = scale) + shift
    ),
    realise = \(n) exp(stats::rgamma(n, shape = shape, scale = scale) + shift),
    .name = "Log Pearson Type III",
    .vtype = "continuous"
  )
}
