#' Log Pearson Type III distribution
#'
#' @inheritParams dlpearson3
#' @returns A Log Pearson Type III distribution.
#' @export
dst_lp3 <- function(locationlog, scalelog, shape) {
  if (scalelog < 0) {
    stop("scalelog cannot be less than 0.")
  }
  if (shape < 0) {
    stop("scalelog cannot be less than 0.")
  }
  distribution(
    parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
    cdf = \(x) plp3(x, meanlog = meanlog, sdlog = sdlog, skew = skew),
    density = \(x) dlp3(x, meanlog = meanlog, sdlog = sdlog, skew = skew),
    quantile = \(p) qlp3(p, meanlog = meanlog, sdlog = sdlog, skew = skew),
    .name = "Log Pearson Type III",
    .vtype = "continuous"
  )
}
