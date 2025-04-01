#' @noRd
eval_skewness_from_network <- function(distribution, tol = 1e-7, ...) {
  checkmate::assert_class(distribution, "dst")
  if (vtype(distribution) != "continuous") {
    stop(
      "Numerical computation for non-continuous distributions is ",
      "not yet supported in this version of distionary."
    )
  }
  mu <- mean(distribution)
  if (is.nan(mu) || is.infinite(mu)) {
    return(NaN)
  }
  sigma <- stdev(distribution)
  if (is.nan(sigma) || is.infinite(sigma)) {
    return(NaN)
  }
  dens <- representation_as_function(distribution, representation = "density")
  integrand <- function(x) ((x - mu) / sigma)^3 * dens(x)
  r <- range(distribution)
  if (r[1] == -Inf && r[2] == Inf) {
    # t(3) distribution mistakenly comes back with finite skewness; break
    # integral into two to solve this issue.
    int1 <- try(
      cubature::hcubature(
        integrand,
        lowerLimit = r[1], upperLimit = 0,
        tol = tol,
        ...
      )$integral,
      silent = TRUE
    )
    r[1] <- 0
  } else {
    int1 <- 0
  }
  int <- try(
    int1 + cubature::hcubature(
      integrand,
      lowerLimit = r[1], upperLimit = r[2],
      tol = tol,
      ...
    )$integral,
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    message(
      "Integration routine for numerical computation of skewness failed. ",
      "This could be because the skewness does not exist. Returning NaN."
    )
    return(NaN)
  }
  int
}
