#' @noRd
eval_mean_from_network <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")

  if (
    attr(distribution, "name") %in%
    c("Hypergeometric", "Bernoulli", "Binomial")
  ) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    r <- range(distribution)
    x <- seq(r[1], r[2], by = 1L)
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x))
  } else if (
    attr(distribution, "name") %in%
      c("Negative Binomial", "Poisson", "Geometric")
  ) {
    to_add <- Inf
    i <- 0
    mean <- 0
    while (to_add > 1e-9) { # Tolerance built-in because only used in tests.
      x <- 1:100 + 100 * i
      to_add <- sum(eval_pmf(distribution, x) * x)
      mean <- mean + to_add
      i <- i + 1
    }
    return(mean)
  } else if (vtype(distribution) != "continuous") {
    stop(
      "Numerical computation for most non-continuous distributions is ",
      "not supported in this version of distionary."
    )
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
    message("Integration routine for numerical computation of mean failed; returning NaN.")
    return(NaN)
  }
  int$value
}
