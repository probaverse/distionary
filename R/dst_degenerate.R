#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param location Parameter of the distribution family.
#' @return Object of class "dst".
#' @examples
#' require(graphics)
#' d <- dst_degenerate(5)
#' plot(d, "quantile")
#' @rdname degenerate
#' @export
dst_degenerate <- function(location) {
  cant_coerce_numeric <- suppressWarnings(is.na(as.numeric(location)))
  if (cant_coerce_numeric) {
    stop("'location' parameter must be numeric.")
  }
  if (length(location) != 1L) {
    stop(
      "'location' parameter must contain exactly one number. ",
      "Received ", length(location)
    )
  }
  distribution(
    parameters = list(location = location),
    cdf = \(x) as.numeric(x <= location),
    quantile = \(p) rep(location, length(p)),
    pmf = \(x) as.numeric(x == location),
    realise = \(n) rep(location, n),
    survival = \(x) as.numeric(x > location),
    mean = location,
    stdev = 0,
    variance = 0,
    skewness = NaN,
    kurtosis_exc = NaN,
    .name = "Degenerate",
    .vtype = "discrete"
  )
}
