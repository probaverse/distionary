validate_range <- function(
    distribution, verbose = FALSE, tol = 1e-05
) {
  if (is.null(distribution[["range"]])) return(NA)
  range_builtin <- distribution$range
  distribution$range <- NULL
  range_derived <- range(distribution)
  left_diff <- abs(range_builtin[1] - range_derived[1])
  right_diff <- abs(range_builtin[2] - range_derived[2])
  if (left_diff < tol || right_diff < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated range differs from stored range by ",
        signif(max(left_diff, right_diff), 4), "."
      )
    }
    return(FALSE)
  }
}
