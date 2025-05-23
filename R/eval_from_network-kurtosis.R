#' @noRd
eval_kurtosis_exc_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  kurtosis(distribution) - 3
}

#' @noRd
eval_kurtosis_from_network <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")
  if (is_intrinsic(distribution, "kurtosis_exc")) {
    kurtosis_exc(distribution) + 3
  } else {
    algorithm_kurtosis(distribution, ...)
  }
}

#' @noRd
algorithm_kurtosis <- function(distribution, tol = 1e-7, ...) {
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
  r <- range(distribution)
  dens <- representation_as_function(distribution, representation = "density")
  integrand <- \(x) ((x - mu) / sigma)^4 * dens(x)
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
      "Integration routine for numerical computation of kurtosis failed. ",
      "This could be because the kurtosis does not exist. Returning NaN."
    )
    return(NaN)
  }
  int
}
