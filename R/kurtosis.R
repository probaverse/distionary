#' @rdname moments
#' @export
kurtosis <- function(distribution) {
  eval_representation(distribution, "kurtosis")
}

#' @rdname moments
#' @export
kurtosis_exc <- function(distribution) {
  eval_representation(distribution, "kurtosis_exc")
}

eval_kurtosis_from_network <- function(distribution) {
  mu <- mean(distribution)
  var <- variance(distribution)
  sf <- distribution[["survival"]]
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  int <- stats::integrate(sf2, 0, Inf)
  int$value / var^2
}

eval_kurtosis_exc_from_network <- function(distribution) {
  kurtosis(distribution) - 3
}
