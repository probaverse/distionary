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
  if (!is_multivariate(distribution)) {
    return(invisible(distribution))
  }
  p <- dimension(distribution)
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

#' Name a multivariate moment after the distribution's variables.
#'
#' A family's moments carry the names it was built with; the distribution's
#' current names are the ones to show, in case it has been renamed since.
#' @param value A vector (one entry per variable) or a square matrix.
#' @noRd
name_by_variables <- function(value, distribution) {
  if (!is_multivariate(distribution) || is.null(value)) {
    return(value)
  }
  vars <- variables(distribution)
  if (is.matrix(value)) {
    dimnames(value) <- list(vars, vars)
  } else {
    names(value) <- vars
  }
  value
}
