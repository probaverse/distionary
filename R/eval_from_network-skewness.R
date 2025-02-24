#' @noRd
eval_skewness_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  # skewness = E[(X - mu)^3] / sigma^3
  # (X - mu)^3 has cdf F(mu + x^(1 / 3)), but note R evaluates (-1)^(1 / 3)
  # as NaN, supposedly grabbing a complex root of unity rather than just -1,
  # so look at positive and negative part of the CDF.
  mu <- mean(distribution)
  sigma <- stdev(distribution)
  if (is.nan(mu) || is.infinite(mu) || is.nan(sigma) || is.infinite(sigma)) {
    return(NaN)
  }
  if (attr(distribution, "name") %in% c(
    "Hypergeometric", "Bernoulli", "Binomial"
  )) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    r <- range(distribution)
    x <- seq(r[1], r[2], by = 1L)
    x3 <- ((x - mu) / sigma)^3
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x3))
  }
  if (attr(distribution, "name") %in% c(
    "Negative Binomial", "Poisson", "Geometric"
  )) {
    to_add <- Inf
    i <- 0
    skew <- 0
    while (to_add > 1e-9) {
      x <- 0:99 + 100 * i
      to_add <- sum(eval_pmf(distribution, x) * ((x - mu) / sigma)^3)
      skew <- skew + to_add
      i <- i + 1
    }
    return(skew)
  }
  sf <- distribution[["survival"]]
  cdf <- distribution[["cdf"]]
  rng <- range(distribution)
  rng2 <- (rng - mu)^3
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf_upper <- function(t) sf(mu + t^(1 / 3))
  cdf_lower <- function(t) cdf(mu - t^(1 / 3))
  # one_minus_flipped <- function(t) cdf(mu - t^(1 / 3)) #1 - sf(mu - t^(1 / 3))
  # (flipped about t=0 because (-1)^(1/3) returns a complex root of
  #  unity, or NaN, instead of the real one, -1.)
  # positive_part <- stats::integrate(sf2, 0, Inf, ...)
  positive_part <- try(
    stats::integrate(
      sf_upper, 0, rng2[2],
      rel.tol = 1e-9, subdivisions = 200L
    ),
    silent = TRUE
  )
  if (inherits(positive_part, "try-error")) {
    return(NaN)
  }
  negative_part <- try(
    stats::integrate(
      cdf_lower, 0, abs(rng2[1]),
      rel.tol = 1e-9, subdivisions = 200L
    ),
    silent = TRUE
  )
  if (inherits(negative_part, "try-error")) {
    return(NaN)
  }
  (positive_part$value - negative_part$value) / sigma^3
}
