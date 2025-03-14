#' Log Normal Distribution
#'
#' Makes a Log Normal distribution, which is the distribution of
#' the exponential of a Normally distributed random variable.
#' @param meanlog Mean of the log of the random variable;
#' single numeric.
#' @param sdlog Standard deviation of the log of the random variable;
#' single positive numeric.
#' @returns A Log Normal distribution.
#' @examples
#' dst_lnorm(0, 1)
#' @export
dst_lnorm <- function(meanlog, sdlog) {
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  if (is.na(meanlog) || is.na(sdlog)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(meanlog = meanlog, sdlog = sdlog),
    density = \(x) stats::dlnorm(x, meanlog = meanlog, sdlog = sdlog),
    cdf = \(x) stats::plnorm(x, meanlog = meanlog, sdlog = sdlog),
    quantile = \(p) stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog),
    realise = \(n) stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog),
    survival = \(x) stats::plnorm(
      x,
      meanlog = meanlog, sdlog = sdlog, lower.tail = FALSE
    ),
    mean = exp(meanlog + sdlog^2 / 2),
    median = exp(meanlog),
    variance = {
      ev <- exp(sdlog^2)
      (ev - 1) * ev * exp(2 * meanlog)
    },
    skewness = {
      ev <- exp(sdlog^2)
      (ev + 2) * sqrt(ev - 1)
    },
    kurtosis_exc = {
      e4 <- exp(4 * sdlog^2)
      e3 <- exp(3 * sdlog^2)
      e2 <- exp(2 * sdlog^2)
      e4 + 2 * e3 + 3 * e2 - 6
    },
    range = c(0, Inf),
    .name = "Log Normal",
    .vtype = "continuous"
  )
}
