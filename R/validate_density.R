#' Validate Distribution Properties
#'
#' If a distribution property is included in a distribution's definition,
#' this function compares its calculation using the embedded
#' definition with the calculation obtained from other properties.
#' @param distribution Probability distribution to validate.
#' @param verbose Print a message if the distribution property is deemed invalid?
#' Single logical; defaults to `FALSE`.
#' @param tol Tolerance; if two calculated properties differ by less than
#' this amount, they are deemed "equal". Single numeric.
#' @returns Single logical indicating whether the property is deemed
#' valid (`TRUE`) or not (`FALSE`). If not applicable (e.g., no such
#' property has been specified for the distribution), `NA` is returned.
#' @noRd
validate_density <- function(distribution, verbose = FALSE, tol = 1e-05) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(tol, 0, len = 1, any.missing = FALSE)
  dens_fun <- distribution$density
  cdf_fun <- distribution$cdf
  if (is.null(dens_fun)) {
    return(NA)
  }
  if (vtype(distribution) != "continuous") {
    warning("Found a density function for a non-continuous distribution.")
    return(FALSE)
  }
  x <- eval_quantile(distribution, at = 1:50 / 50)
  rng <- range(distribution)
  cdf_evald <- eval_cdf(distribution, at = x)
  cdf_derived1 <- stats::integrate(
    dens_fun,
    lower = rng[1], upper = x[1]
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
