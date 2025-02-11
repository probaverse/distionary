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
  k_exc <- distribution$kurtosis_exc
  if (!is.null(k_exc)) return(k_exc + 3)
  mu <- mean(distribution)
  var <- variance(distribution)
  sf <- distribution[["survival"]]
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  int <- try(
    stats::integrate(sf2, 0, Inf, rel.tol = 1e-9, subdivisions = 200L),
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    return(NaN)
  }
  int$value / var^2
}

eval_kurtosis_exc_from_network <- function(distribution) {
  kurtosis(distribution) - 3
}
