#' Geometric Distribution
#'
#' Makes a Geometric distribution, corresponding to the number of failures
#' in a sequence of independent trials before observing a success.
#'
#' @param prob Probability of success in each trial; single numeric
#' between 0 and 1.
#' @returns A Geometric distribution.
#' @examples
#' d <- dst_geom(0.4)
#'
#' # This version of the Geometric distribution does not count the success.
#' range(d)
#' @export
dst_geom <- function(prob) {
  checkmate::assert_numeric(prob, 0, 1, len = 1)
  if (is.na(prob)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(prob = prob),
    pmf = \(x) stats::dgeom(x, prob = prob),
    cdf = \(x) stats::pgeom(x, prob = prob),
    quantile = \(p) stats::qgeom(p, prob = prob),
    realise = \(n) stats::rgeom(n, prob = prob),
    survival = \(x) stats::pgeom(x, prob = prob, lower.tail = FALSE),
    mean = (1 - prob) / prob,
    variance = (1 - prob) / prob^2,
    skewness = ifelse(prob < 1, (2 - prob) / sqrt(1 - prob), NaN),
    kurtosis_exc = ifelse(prob < 1, 6 + prob^2 / (1 - prob), NaN),
    range = c(0, Inf),
    .name = "Geometric",
    .vtype = "discrete"
  )
}
