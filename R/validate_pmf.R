#' @family validate_properties
#' @inheritParams validate_density
#' @noRd
validate_pmf <- function(distribution, verbose = FALSE, tol = 1e-8) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(tol, 0, len = 1, any.missing = FALSE)
  pmf_fun <- distribution$pmf
  cdf_fun <- distribution$cdf
  if (is.null(pmf_fun)) {
    return(NA)
  }
  if (vtype(distribution) != "discrete") {
    warning("Found a PMF for a non-discrete distribution.")
    return(FALSE)
  }
  x <- 0:1000
  pmf_evald <- eval_pmf(distribution, at = x)
  pmf_derived <- prob_left(distribution, of = x, inclusive = TRUE) -
    prob_left(distribution, of = x, inclusive = FALSE)
  diffs <- abs(pmf_derived - pmf_evald)
  if (all(diffs < tol)) {
    return(TRUE)
  } else {
    if (verbose) {
      i <- which(diffs == max(diffs))[1]
      message(
        "Invalid PMF, evaluating to a difference of ",
        signif(max(diffs), 4), " at x = ", x[i], "."
      )
    }
    return(FALSE)
  }
}
