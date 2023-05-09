#' Convert Distributions in Ellipsis to List
#'
#' Flattens distributions placed in an ellipsis argument into a list,
#' so that the ellipsis can include distributions themselves and lists
#' of distributions. `dots_to_quos()` returns a squashed list of quosures;
#' `dots_to_dsts()` returns a squashed list of distributions.
#'
#' @param na.rm Logical; remove NA entries? Note that NULL entries are
#' always removed.
#' @inheritParams enframe_cdf
#' @return A list of distributions contained in the `...`, with NULL
#' entries discarded. If no distributions are present, returns `list()`.
#' @details An error is thrown if, after discarding NULL entries,
#' `...` contains non-distributions. This function is essentially a
#' wrapper around `rlang::squash()`.
#' @examples
#' d <- dst_norm(0, 1)
#' distplyr:::dots_to_dsts(d, list(d, d), NULL)
#' @rdname dots_to
dots_to_quos <- function(...) {
  dsts <- rlang::enquos(...)
  rlang::squash(dsts)
}

#' @rdname dots_to
dots_to_dsts <- function(..., na.rm = FALSE) {
  dsts <- dots_to_quos(...)
  dsts <- lapply(dsts, rlang::eval_tidy)
  nulls <- vapply(dsts, is.null, FUN.VALUE = logical(1L))
  is_na <- function(x) length(x) == 1L && is.na(x)
  if (na.rm) {
    nulls <- nulls | vapply(dsts, is_na, FUN.VALUE = logical(1L))
    acceptable_entry <- is_distribution
  } else {
    acceptable_entry <- function(x) is_distribution(x) || is_na(x)
  }
  dsts <- dsts[!nulls]
  dsts
}


