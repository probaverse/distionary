#' @noRd
eval_variance_from_network <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")
  if (is_intrinsic(distribution, "stdev")) {
    stdev(distribution)^2
  } else {
    algorithm_variance(distribution, ...)
  }
}

#' @noRd
algorithm_variance <- function(distribution, tol = 1e-9, ...) {
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
  dens <- representation_as_function(distribution, representation = "density")
  integrand <- function(x) (x - mu)^2 * dens(x)
  r <- range(distribution)
  int <- try(
    distionary_integrate(
      integrand,
      lower = r[1], upper = r[2],
      tol = tol,
      ...
    ),
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    message(
      "Integration routine for numerical computation of variance failed. ",
      "This could be because the variance does not exist. Returning NaN."
    )
    return(NaN)
  }
  int
}
