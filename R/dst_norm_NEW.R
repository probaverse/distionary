dst_norm <- function(mu, sigma) {
  distribution(
    density = \(x) stats::dnorm(x, mean = mu, sd = sigma),
    cdf = \(x) stats::pnorm(x, mean = mu, sd = sigma),
    survival = \(x) stats::pnorm(x, mean = mu, sd = sigma, lower.tail = FALSE),
    quantile = \(p) stats::qnorm(p, mean = mu, sd = sigma),
    realise = \(n) stats::rnorm(n, mean = mu, sd = sigma),
    mean = mu,
    stdev = sigma,
    variance = sigma^2,
    skewness = 0,
    kurtosis_exc = 0,
    kurtosis_raw = 3,
    .parameters = paramspace(mu, sigma, sigma > 0)
  )
}
