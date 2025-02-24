#' Generalised Pareto Distribution
#'
#' Makes a Generalized Pareto distribution (GPD), corresponding to the
#' limiting distribution of excesses over a threshold.
#' @param scale Scale parameter; single positive numeric.
#' @param shape Shape parameter; single positive numeric.
#' This is also the extreme value index, so that `shape > 0` is heavy
#' tailed, and `shape < 0` is short-tailed.
#' @return A Generalised Pareto Distribution.
#' @examples
#' # Short-tailed example
#' short <- dst_gpd(1, -1)
#' range(short)
#' mean(short)
#'
#' # Heavy-tailed example
#' heavy <- dst_gpd(1, 1)
#' range(heavy)
#' mean(heavy)
#'
#' # Light-tailed example (a Gumbel distribution)
#' light <- dst_gpd(1, 0)
#' range(light)
#' mean(light)
#' @export
dst_gpd <- function(scale, shape) {
  checkmate::assert_numeric(scale, 0, len = 1)
  checkmate::assert_numeric(shape, len = 1)
  if (is.na(scale) || is.na(shape)) {
    return(dst_null())
  }
  distribution(
    parameters = list(scale = scale, shape = shape),
    cdf = \(x) pgpd(x, scale = scale, shape = shape),
    survival = \(x) pgpd(
      x,
      scale = scale, shape = shape, lower.tail = FALSE
    ),
    quantile = \(p) qgpd(p, scale = scale, shape = shape),
    density = \(x) dgpd(x, scale = scale, shape = shape),
    mean = ifelse(shape < 1, scale / (1 - shape), Inf),
    variance = ifelse(
      shape < 1 / 2,
      scale^2 / (1 - shape)^2 / (1 - 2 * shape),
      Inf
    ),
    skewness = ifelse(
      shape < 1 / 3,
      2 * (1 + shape) * sqrt(1 - 2 * shape) /
        (1 - 3 * shape),
      Inf
    ),
    kurtosis_exc = ifelse(
      shape < 1 / 4,
      3 * (1 - 2 * shape) * (2 * shape^2 + shape + 3) /
        ((1 - 3 * shape) * (1 - 4 * shape)) - 3,
      Inf
    ),
    range = c(0, gpd_upper(scale = scale, shape = shape)),
    .name = "Generalised Pareto",
    .vtype = "continuous"
  )
}
