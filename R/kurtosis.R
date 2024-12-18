#' @rdname moments
#' @export
kurtosis_raw <- function(distribution, ...) {
  UseMethod("kurtosis_raw")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution, ...) {
  UseMethod("kurtosis_exc")
}

#' @export
kurtosis_raw.dst <- function(distribution, ...) {
  3 + kurtosis_exc(distribution, ...)
}

#' @export
kurtosis_exc.dst <- function(distribution, ...) {
  mu <- mean(distribution, ...)
  var <- variance(distribution, ...)
  sf <- representation_as_function(distribution, "survival")
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  int <- stats::integrate(sf2, 0, Inf, ...)
  int$value / var^2 - 3
}
