#' Generalized Pareto Distribution
#'
#' Makes a distribution belonging to the family of
#' generalized Pareto distributions (GPD).
#' @param scale Scale parameter; positive numeric.
#' @param shape Shape parameter; numeric.
#' @return Object of class "dst" of a GPD.
#' @examples
#' dst_gpd(1, 1)
#' @export
dst_gpd <- function(scale, shape) {
	if (scale <= 0) {
		stop("'scale' parameter must be positive.")
	}
  distribution(
    parameters = list(scale = scale, shape = shape),
    cdf = \(x) pgpd(
      x, location = 0, scale = scale, shape = shape
    ),
    quantile = \(p) qgpd(p, location = 0, scale = scale, shape = shape),
    density = \(x) dgpd(p, location = 0, scale = scale, shape = shape),
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
