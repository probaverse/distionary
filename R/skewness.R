#' @rdname moments
#' @export
skewness <- function(distribution, ...) UseMethod("skewness")

#' @export
skewness.dst <- function(distribution, ...) {
  mu <- mean(distribution, ...)
  sigma <- stdev(distribution, ...)
  sf <- representation_as_function(distribution, "survival")
  sf2 <- function(t) sf(mu + t^(1 / 3))
  one_minus_flipped <- function(t) 1 - sf(mu - t^(1 / 3))
  # (flipped about t=0 because (-1)^(1/3) returns a complex root of
  #  unity, or NaN, instead of the real one, -1.)
  # pos_int <- stats::integrate(sf2, 0, Inf, ...)
  if (inherits(
    try(stats::integrate(sf2, 0, Inf, ...), silent = TRUE),
    "try-error"
  )) {
    warning("Integral did not converge. This might mean that the mean does
            not exist, or that the integral simply did not converge.
            Returning NaN.")
    return(NaN)
  } else {
    pos_int <- stats::integrate(sf2, 0, Inf, ...)
  }
  if (inherits(
    try(stats::integrate(one_minus_flipped, 0, Inf, ...), silent = TRUE),
    "try-error"
  )) {
    warning("Integral did not converge. This might mean that the mean does
            not exist, or that the integral simply did not converge.
            Returning NaN.")
    return(NaN)
  } else {
    neg_int <- stats::integrate(one_minus_flipped, 0, Inf, ...)
  }
  (pos_int$value - neg_int$value) / sigma^3
}
