#' Convert a data frame to a tibble
#'
#' Converts a data frame to a tibble, if the user has the `tibble`
#' package installed.
#' @param res Data frame.
#' @returns A tibble, if the user has the `tibble` package installed.
#' @noRd
convert_dataframe_to_tibble <- function(res) {
  checkmate::assert_data_frame(res)
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  res
}

#' Refuse a multivariate distribution in a function for one variable.
#'
#' @param distribution A distribution.
#' @param fn Name of the calling function, without parentheses.
#' @param alt Stem of the multivariate counterparts, if there are any (such as
#' `"cdf"` for `eval_bi_cdf()` and `eval_mv_cdf()`).
#' @noRd
assert_univariate <- function(distribution, fn, alt = NULL) {
  p <- dimension(distribution)
  if (is.na(p) || p == 1L) {
    return(invisible(distribution))
  }
  instead <- if (is.null(alt)) {
    "Take one variable with `marginal()` first."
  } else {
    paste0(
      "Use `eval_bi_", alt, "()` or `eval_mv_", alt, "()`, or take one\n",
      "variable with `marginal()` first."
    )
  }
  stop(
    "`", fn, "()` is for distributions of one variable, and this one\n",
    "has ", p, ". ", instead,
    call. = FALSE
  )
}
