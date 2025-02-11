validate_survival <- function(
    distribution, verbose = FALSE, tol = .Machine$double.eps
) {
  if (is.null(distribution$survival)) return(NA)
  sf <- distribution$survival
  distribution$survival <- NULL
  x <- eval_quantile(distribution, at = 1:49 / 50)
  sf_builtin <- sf(x)
  sf_derived <- eval_survival(distribution, at = x)
  diffs <- abs(sf_builtin - sf_derived)
  if (all(diffs < tol)) {
    return(TRUE)
  } else {
    if (verbose) {
      i <- which(diffs == max(diffs))[1]
      message(
        "Invalid survival function, evaluating to a difference of ",
        signif(max(diffs), 4), "at x = ", x[i], "."
      )
    }
    return(FALSE)
  }
}
