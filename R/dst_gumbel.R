#' Gumbel Distribution
#'
#' Makes a Gumbel distribution, which is a special case of the
#' Generalised Extreme Value (GEV) distribution when the shape parameter is 0.
#' @param location Location parameter; single numeric.
#' @param scale Scale parameter; single positive numeric.
#' @returns A Gumbel distribution.
#' @examples
#' dst_gumbel(0, 1)
#' @export
dst_gumbel <- function(location, scale) {
  checkmate::assert_numeric(location, len = 1)
  checkmate::assert_numeric(scale, 0, len = 1)
  if (is.na(location) || is.na(scale)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(location = location, scale = scale),
    density = function(x) {
      dgev(x, location = location, scale = scale, shape = 0)
    },
    cdf = function(x) {
      pgev(x, location = location, scale = scale, shape = 0)
    },
    quantile = function(p) {
      qgev(p, location = location, scale = scale, shape = 0)
    },
    mean = location - scale * digamma(1),
    variance = scale^2 * pi^2 / 6,
    range = c(-Inf, Inf),
    .name = "Gumbel",
    .vtype = "continuous"
  )
}
