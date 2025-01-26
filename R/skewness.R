#' Skewness of a Distribution
#'
#' @param distribution Distribution to compute skewness from.
#'
#' @rdname moments
#' @export
skewness <- function(distribution) {
  eval_representation(distribution, "skewness")
}

eval_skewness_from_network <- function(distribution) {
  mu <- mean(distribution)
  sigma <- stdev(distribution)
  sf <- distribution[["survival"]]
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf2 <- function(t) sf(mu + t^(1 / 3))
  one_minus_flipped <- function(t) 1 - sf(mu - t^(1 / 3))
  # (flipped about t=0 because (-1)^(1/3) returns a complex root of
  #  unity, or NaN, instead of the real one, -1.)
  # pos_int <- stats::integrate(sf2, 0, Inf, ...)
  pos_int <- try(stats::integrate(sf2, 0, Inf), silent = TRUE)
  if (inherits(pos_int, "try-error")) {
    warning(
      "Integral did not converge. This might mean that the skewness does ",
      "not exist, or that the integral simply did not converge. ",
      "Returning `NaN`."
    )
    return(NaN)
  }
  neg_int <- try(stats::integrate(one_minus_flipped, 0, Inf), silent = TRUE)
  if (inherits(neg_int, "try-error")) {
    warning(
      "Integral did not converge. This might mean that the skewness does ",
      "not exist, or that the integral simply did not converge. ",
      "Returning `NaN`."
    )
    return(NaN)
  }
  (pos_int$value - neg_int$value) / sigma^3
}
