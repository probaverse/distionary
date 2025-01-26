#' @rdname moments
#' @export
kurtosis_raw <- function(distribution) {
  eval_representation(distribution, "kurtosis_raw")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  eval_representation(distribution, "kurtosis_exc")
}

kurtosis_raw_from_network <- function(distribution) {
  3 + kurtosis_exc(distribution)
}

kurtosis_exc_from_network <- function(distribution, ...) {
  mu <- mean(distribution)
  var <- variance(distribution)
  sf <- distribution[["survival"]]
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  int <- stats::integrate(sf2, 0, Inf, ...)
  int$value / var^2 - 3
}
