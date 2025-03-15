#' @noRd
validate_quantile <- function(distribution, verbose = FALSE, tol = 1e-06) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(tol, 0, len = 1, any.missing = FALSE)
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
