#' @noRd
eval_mean_from_network <- function(distribution, tol = 1e-9, ...) {
  checkmate::assert_class(distribution, "dst")
  if (vtype(distribution) != "continuous") {
    stop(
      "Numerical computation for non-continuous distributions is ",
      "not yet supported in this version of distionary."
    )
  }
  dens <- representation_as_function(distribution, representation = "density")
  integrand <- function(x) x * dens(x)
  r <- range(distribution)
  distionary_integrate(integrand, lower = r[1], upper = r[2], tol = tol, ...)
}
