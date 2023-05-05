#' Check distributions input
#'
#' Checks whether a collection of objects are either distributions,
#' NA, or NULL. If one object does not satisy these requirements,
#' an error is thrown.
#'
#' @param distributions List of objects to check. Should be flattened if
#' coming from `...`.
#' @return Invisible; ran for the potential error.
check_dsts <- function(distributions) {
  bad_dots <- vapply(distributions, function(dst) {
    !is_distribution(dst) && !is.na(dst) && !is.null(dst)
  }, FUN.VALUE = logical(1L))
  if (any(bad_dots)) {
    stop("Ellipsis can only contain distributions or NULL/NA values. ",
         "Bad entries: ", paste(which(bad_dots), collapse = ", "), ".")
  }
  invisible()
}
