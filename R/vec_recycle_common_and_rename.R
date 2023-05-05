#' vec_recycle_common and fill in missing names
#'
#' Recycles input vectors to a common length, as in
#' `vctrs::vec_recycle_common()`, but names that were originally unnamed
#' are given names: when an input is a symbol, that symbol becomes the
#' component name; otherwise, the name is `...i`, where `i` is the entry
#' number.
#' @param ... Vectors to recycle.
#' @return A named list, not necessarily with unique names.
#' @examples
#' a <- 1:3
#' distionary:::vec_recycle_common_and_rename(a, b = 2, 32 + 5, f = a, f = 4)
#' distionary:::vec_recycle_common_and_rename(!!!list(a, b = 2, 32 + 5))
vec_recycle_common_and_rename <- function(...) {
  ellipsis <- rlang::enexprs(...)
  .l <- vctrs::vec_recycle_common(...)
  need_names <- !rlang::have_name(ellipsis)
  if (any(need_names)) {
    n_need_names <- sum(need_names)
    names(.l)[need_names] <- paste0("...", 1:length(.l))[need_names]
    are_symbols <- vapply(ellipsis, rlang::is_symbol, FUN.VALUE = logical(1L))
    names(.l)[are_symbols & need_names] <- vapply(
      ellipsis[are_symbols & need_names], rlang::as_string,
      FUN.VALUE = character(1L)
    )
  }
  .l
}
