validate_density <- function(distribution, verbose = FALSE, tol = 1e-06) {
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
  cdf_derived <- vapply(
    x, function(x_) {
      int <- stats::integrate(dens_fun, lower = rng[1], upper = x_)
      int$value
    },
    FUN.VALUE = numeric(1)
  )
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

