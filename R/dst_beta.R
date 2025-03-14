#' Beta Distribution
#'
#' Makes a Beta distribution.
#' @param shape1,shape2 Shape parameters of the distribution;
#' single positive numerics.
#' @returns A Beta distribution.
#' @examples
#' dst_beta(2, 3)
#' @export
dst_beta <- function(shape1, shape2) {
  checkmate::assert_numeric(shape1, 0, len = 1)
  checkmate::assert_numeric(shape2, 0, len = 1)
  if (is.na(shape1) || is.na(shape2)) {
    return(dst_null())
  }
  distribution(
    .parameters = list(shape1 = shape1, shape2 = shape2),
    density = \(x) stats::dbeta(x, shape1, shape2),
    cdf = \(x) stats::pbeta(x, shape1, shape2),
    quantile = \(p) stats::qbeta(p, shape1, shape2),
    realise = \(n) stats::rbeta(n, shape1, shape2),
    survival = \(x) stats::pbeta(x, shape1, shape2, lower.tail = FALSE),
    mean = shape1 / (shape1 + shape2),
    variance = shape1 * shape2 / (shape1 + shape2)^2 /
      (shape1 + shape2 + 1),
    skewness = 2 * (shape2 - shape1) * sqrt(shape1 + shape2 + 1) /
      (shape1 + shape2 + 2) / sqrt(shape1 * shape2),
    range = c(0, 1),
    .vtype = "continuous",
    .name = "Beta"
  )
}
