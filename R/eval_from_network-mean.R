#' @noRd
eval_mean_from_network <- function(distribution, ...) {
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
  if (r[1] == -Inf && r[2] == Inf) {
    # Cauchy distribution mistakenly comes back with finite mean; break
    # integral into two to solve this issue.
    int1 <- try(
      stats::integrate(
        integrand,
        lower = r[1], upper = 0,
        rel.tol = 1e-09,
        subdivisions = 200L,
        ...
      )$value,
      silent = TRUE
    )
    r[1] <- 0
  } else {
    int1 <- 0
  }
  int <- try(
    int1 + stats::integrate(
      integrand,
      lower = r[1], upper = r[2],
      rel.tol = 1e-09,
      subdivisions = 200L,
      ...
    )$value,
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    message(
      "Integration routine for numerical computation of mean failed. ",
      "This could be because the mean does not exist. Returning NaN."
    )
    return(NaN)
  }
  int
}
