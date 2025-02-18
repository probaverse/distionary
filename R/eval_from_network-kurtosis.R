#' @noRd
eval_kurtosis_from_network <- function(distribution) {
  k_exc <- distribution[["kurtosis_exc"]]
  if (!is.null(k_exc)) {
    return(k_exc + 3)
  }
  mu <- mean(distribution)
  sigma <- stdev(distribution)
  r <- range(distribution)
  if (is.nan(mu) || is.infinite(mu) || is.nan(sigma) || is.infinite(sigma)) {
    return(NaN)
  }
  if (attr(distribution, "name") %in% c(
    "Hypergeometric", "Bernoulli", "Binomial"
  )) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    x <- seq(r[1], r[2], by = 1L)
    x4 <- ((x - mu) / sigma)^4
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x4))
  }
  if (attr(distribution, "name") %in% c(
    "Negative Binomial", "Poisson", "Geometric"
  )) {
    to_add <- Inf
    i <- 0
    kur <- 0
    while (to_add > 1e-9) {
      x <- 0:99 + 100 * i
      to_add <- sum(eval_pmf(distribution, x) * ((x - mu) / sigma)^4)
      kur <- kur + to_add
      i <- i + 1
    }
    return(kur)
  }
  if (vtype(distribution) == "continuous") {
    integrand <- \(x) ((x - mu) / sigma)^4 * eval_density(distribution, at = x)
    int <- try(
      stats::integrate(
        integrand,
        lower = r[1], upper = r[2], rel.tol = 1e-9,
        subdivisions = 200L
      ),
      silent = TRUE
    )
    if (inherits(int, "try-error")) {
      return(NaN)
    }
    return(int$value)
  }
  var <- variance(distribution)
  sf <- distribution[["survival"]]
  if (is.null(sf)) {
    sf <- \(x) eval_survival(distribution, at = x)
  }
  sf2 <- function(t) 1 + sf(mu + t^(1 / 4)) - sf(mu - t^(1 / 4))
  eps <- Inf
  low <- 0
  high <- 100
  numerator <- 0
  last_numerator_increment <- Inf
  print(pretty_name(distribution, param_digits = 3))
  while (eps > 1e-9) {
    int <- try(
      stats::integrate(sf2, low, high, rel.tol = 1e-9, subdivisions = 200L),
      silent = TRUE
    )
    if (inherits(int, "try-error")) {
      return(NaN)
    }
    numerator_increment <- int$value
    numerator <- numerator + numerator_increment
    slope <- max(
      -abs(numerator_increment - last_numerator_increment) / (high - low),
      -1
    )
    low <- high
    high <- max(high + 100, high - numerator_increment / slope)
    last_numerator_increment <- numerator_increment
    eps <- numerator_increment / var^2
  }
  numerator / var^2
}

#' @noRd
eval_kurtosis_exc_from_network <- function(distribution) {
  kurtosis(distribution) - 3
}
