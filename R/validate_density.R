validate_density <- function(distribution, verbose = FALSE, tol = 1e-05) {
  dens_fun <- distribution$density
  cdf_fun <- distribution$cdf
  if (is.null(dens_fun)) return(NA)
  if (vtype(distribution) != "continuous") {
    warning("Found a density function for a non-continuous distribution.")
    return(FALSE)
  }
  x <- eval_quantile(distribution, at = 1:50 / 50)
  rng <- range(distribution)
  cdf_evald <- eval_cdf(distribution, at = x)
  cdf_derived1 <- stats::integrate(
    dens_fun, lower = rng[1], upper = x[1]
  )$value
  cdf_derived2 <- vapply(
    x[-1], function(x_) {
      int <- stats::integrate(dens_fun, lower = x[1], upper = x_)
      int$value
    },
    FUN.VALUE = numeric(1)
  ) + cdf_derived1
  cdf_derived <- append(cdf_derived1, cdf_derived2)
  diffs <- abs(cdf_derived - cdf_evald)
  if (all(diffs < tol)) {
    return(TRUE)
  } else {
    if (verbose) {
      i <- which(diffs == max(diffs))[1]
      message(
        "Invalid density function, evaluating to a difference of ",
        signif(max(diffs), 4), " at x = ", x[i], "."
      )
    }
    return(FALSE)
  }
}

