#' Geometric Distribution
#'
#' Makes a distribution belonging to the family of
#' geometric distributions.
#'
#' @param prob Probability of success in each trial; 0 < `p` <= 1.
#' @examples
#' dst_geom(0.4)
#' @export
dst_geom <- function(prob) {
  if (prob <= 0 || prob > 1) {
    stop('prob must be greater than 0 and less than or equal to 1.')
  }
  distribution(
    parameters = list(prob = prob),
    density = \(x) stats::dgeom(x, prob = prob),
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
