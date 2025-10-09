#' @noRd
eval_skewness_from_network <- function(distribution, tol = 1e-9, ...) {
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
  int <- try(
    distionary_integrate(integrand, r[1], r[2], tol = tol, ...),
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
