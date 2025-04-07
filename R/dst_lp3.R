#' Log Pearson Type III distribution
#'
#' Makes a Log Pearson Type III distribution, which is the
#' distribution of the exponential of a random variable following
#' a Pearson Type III distribution.
#'
#' @param meanlog Mean of the log of the random variable; single numeric.
#' @param sdlog Standard deviation of the log of the random variable;
#' single positive numeric.
#' @param skew Skewness of the log of the random variable;
#' single numeric.
#' @returns A Log Pearson Type III distribution.
#' @examples
#' dst_lp3(0, 1, 1)
#' @srrstats {PD3.0} *Manipulation of probability distributions should
#' very generally be analytic, with numeric manipulations only
#' implemented with clear justification (ideally including references).*
#' This only applies to the Pearson Type III and Log Pearson Type III
#' that manipulates the Gamma distribution from the stats package analytically.
#' User-facing manipulation is the job of the `distplyr` package in the
#' probaverse family. --> Copied to `dst_lp3.R`.
#' @export
dst_lp3 <- function(meanlog, sdlog, skew) {
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  checkmate::assert_numeric(skew, len = 1)
  if (is.na(meanlog) || is.na(sdlog) || is.na(skew)) {
    return(dst_null())
  }
  # skewness = 2 / sqrt(shape), shape = 4 / skewness^2
  shape <- 4 / skew^2
  # sd = sqrt(shape) * scale
  scale <- sdlog / sqrt(shape)
  # mean = scale * shape
  shift <- meanlog - scale * shape
  cdf_gamma <-
    distribution(
      .parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
      cdf = \(x) stats::pgamma(
        log(pmax(0, x)) - shift,
        shape = shape, scale = scale
      ),
      survival = \(x) stats::pgamma(
        log(pmax(0, x)) - shift,
        shape = shape, scale = scale, lower.tail = FALSE
      ),
      density = \(x) {
        res <- stats::dgamma(
          log(pmax(0, x)) - shift,
          shape = shape, scale = scale
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
