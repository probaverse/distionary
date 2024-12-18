#' @rdname moments
#' @export
variance <- function(distribution, ...) {
  UseMethod("variance")
}

#' @export
variance.dst <- function(distribution, ...) {
  mu <- mean(distribution, ...)
  sf <- representation_as_function(distribution, "survival")
  sf2 <- function(t) 1 + sf(mu + sqrt(t)) - sf(mu - sqrt(t))
  int <- stats::integrate(sf2, 0, Inf, ...)
  if (inherits(int, "try-error")) {
    warning(
      "Integral did not converge. This might mean that the mean does",
      "not exist, or that the integral simply did not converge.",
      "Returning NaN."
    )
    return(NaN)
  }
  int$value
}
