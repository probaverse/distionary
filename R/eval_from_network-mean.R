#' @srrstats {PD3.5} *Integration routines should only rely on
#' discrete summation where such use can be justified (for example,
#' through providing a literature reference), in which case the
#' following applies:*
#' - Discrete summation is only used for specific distributions.
#' @noRd
eval_mean_from_network <- function(distribution, ...) {
  if (attr(distribution, "name") %in% c(
    "Hypergeometric", "Bernoulli", "Binomial"
  )) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    r <- range(distribution)
    x <- seq(r[1], r[2], by = 1L)
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x))
  }
  if (attr(distribution, "name") %in% c(
    "Negative Binomial", "Poisson", "Geometric"
  )) {
    to_add <- Inf
    i <- 0
    mean <- 0
    while (to_add > 1e-9) {
      x <- 1:100 + 100 * i
      to_add <- sum(eval_pmf(distribution, x) * x)
      mean <- mean + to_add
      i <- i + 1
    }
    return(mean)
  }
  qf <- distribution[["quantile"]]
  if (is.null(qf)) {
    qf <- \(x) eval_quantile_from_network(distribution, x)
  }
  int <- try(
    stats::integrate(
      qf,
      lower = 0, upper = 1, rel.tol = 1e-09, subdivisions = 200L, ...
    ),
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    return(NaN)
  }
  int$value
}
