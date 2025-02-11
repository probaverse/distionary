#' @rdname moments
#' @export
variance <- function(distribution) {
  eval_representation(distribution, "variance")
}

eval_variance_from_network <- function(distribution) {
  mu <- mean(distribution)
  # sf <- representation_as_function(distribution, "survival")
  # sf2 <- function(t) 1 + sf(mu + sqrt(t)) - sf(mu - sqrt(t))
  # P((X - mu)^2 < x) = P(mu-sqrt(x) < X < mu+sqrt(x))
  # = F(mu + sqrt(x)) - F(mu - sqrt(x))
  sf2 <- function(x) eval_survival(distribution, at = mu + sqrt(x)) +
    eval_cdf(distribution, mu - sqrt(x))
  int <- try(
    stats::integrate(sf2, 0, Inf, rel.tol = 1e-9, subdivisions = 200L),
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    return(NaN)
  }
  int$value
}
