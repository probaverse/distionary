#' Check distributions input
#'
#' Checks whether a collection of objects are either distributions,
#' NA, or NULL. If one object does not satisy these requirements,
#' an error is thrown.
#'
#' @param distributions List of objects to check. Should be flattened if
#' coming from `...` upstream.
#' @return Invisible; ran for the potential error.
check_dsts <- function(distributions) {
  not_dsts <- vapply(distributions, function(dst) {
    !is_distribution(dst) && !all(is.na(dst)) && !is.null(dst)
  }, FUN.VALUE = logical(1L))
  if (any(not_dsts)) {
    stop("Bad distribution entries:", paste(which(not_dsts), collapse = ", "),
         ".")
  }
  dims <- vapply(distributions, dimension, FUN.VALUE = numeric(1L))
  if (length(unique(dims)) > 1) {
    stop("Distributions must all have the same dimension. Dimensions ",
         "received: ", paste(dims, collapse = ", "), ".")
  }
  invisible()
}
