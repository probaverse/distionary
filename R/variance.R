#' @rdname moments
#' @export
variance <- function(distribution) {
  eval_representation(distribution, "variance")
}

eval_variance_from_network <- function(distribution) {
  mu <- mean(distribution)
  if (attr(distribution, "name") %in% c(
    "Hypergeometric", "Bernoulli", "Binomial"
  )) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    r <- range(distribution)
    x <- seq(r[1], r[2], by = 1L)
    x2 <- (x - mu)^2
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x2))
  }
  if (attr(distribution, "name") %in% c(
    "Negative Binomial", "Poisson", "Geometric"
  )) {
    to_add <- Inf
    i <- 0
    variance <- 0
    while (to_add > 1e-9) {
      x <- 0:99 + 100 * i
      to_add <- sum(eval_pmf(distribution, x) * (x - mu)^2)
      variance <- variance + to_add
      i <- i + 1
    }
    return(variance)
  }
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
