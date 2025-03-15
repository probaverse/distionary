#' @noRd
validate_range <- function(distribution, verbose = FALSE, tol = 1e-05) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(tol, 0, len = 1, any.missing = FALSE)
  if (is.null(distribution[["range"]])) {
    return(NA)
  }
  range_builtin <- distribution$range
  distribution$range <- NULL
  range_derived <- range(distribution)
  # Can't take the difference between to infinite values, so
  # first check equality as a way around.
  if (range_builtin[1] == range_derived[1]) {
    left_diff <- 0
  } else {
    left_diff <- abs(range_builtin[1] - range_derived[1])
  }
  if (range_builtin[2] == range_derived[2]) {
    right_diff <- 0
  } else {
    right_diff <- abs(range_builtin[2] - range_derived[2])
  }
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
