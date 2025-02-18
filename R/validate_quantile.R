#' @family validate_properties
#' @noRd
validate_quantile <- function(
    distribution, verbose = FALSE, tol = 1e-06) {
  if (is.null(distribution$quantile)) {
    return(NA)
  }
  qf <- distribution$quantile
  distribution$quantile <- NULL
  p <- 1:49 / 50
  qf_builtin <- qf(p)
  qf_derived <- eval_quantile(distribution, at = p)
  diffs <- abs(qf_builtin - qf_derived)
  if (all(diffs < tol)) {
    return(TRUE)
  } else {
    if (verbose) {
      i <- which(diffs == max(diffs))[1]
      message(
        "Invalid survival function, evaluating to a difference of ",
        signif(max(diffs), 4), " at p = ", p[i], "."
      )
    }
    return(FALSE)
  }
}
eval_quantile_from_network
