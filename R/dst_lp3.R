#' Log Pearson Type III distribution
#'
#' Makes a Log Pearson Type III distribution, which is the
#' distribution of the exponential of a random variable following
#' a Pearson Type III distribution.
#'
#' @param meanlog,sdlog,skew Parameters.
#' @returns A Log Pearson Type III distribution.
#' @examples
#' dst_lp3(0, 1, 1)
#'
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
  shift <- meanlog - scale * shape
  cdf_gamma <-
  distribution(
    parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
    cdf = \(x) stats::pgamma(
      log(pmax(0, x)) - shift, shape = shape, scale = scale
    ),
    survival = \(x) stats::pgamma(
      log(pmax(0, x)) - shift, shape = shape, scale = scale, lower.tail = FALSE
    ),
    density = \(x) {
      res <- stats::dgamma(
        log(pmax(0, x)) - shift, shape = shape, scale = scale
      ) / x
      res[x == 0] <- 0
      res
    },
    quantile = \(p) exp(
      stats::qgamma(p, shape = shape, scale = scale) + shift
    ),
    realise = \(n) exp(stats::rgamma(n, shape = shape, scale = scale) + shift),
    .name = "Log Pearson Type III",
    .vtype = "continuous"
  )
}
