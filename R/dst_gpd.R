#' Generalised Pareto Distribution
#'
#' Makes a Generalized Pareto distribution (GPD), corresponding to the
#' limiting distribution of excesses over a threshold.
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric. Also the extreme value index,
#' so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.
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
  if (scale <= 0) {
    stop("'scale' parameter must be positive.")
  }
  distribution(
    parameters = list(scale = scale, shape = shape),
    cdf = \(x) pgpd(
      x,
      location = 0, scale = scale, shape = shape
    ),
    survival = \(x) pgpd(
      x,
      location = 0, scale = scale, shape = shape, lower.tail = FALSE
    ),
    quantile = \(p) qgpd(p, location = 0, scale = scale, shape = shape),
    density = \(x) dgpd(x, location = 0, scale = scale, shape = shape),
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
    range = c(0, ifelse(shape >= 0, Inf, -(scale / shape))),
    .name = "Generalised Pareto",
    .vtype = "continuous"
  )
}
