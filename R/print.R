#' @export
print.dst <- function(x, ...) {
  nm <- distribution_name(x)
  if (is.null(nm)) {
    nm <- "Unnamed distribution."
  }
  cat(nm)
  invisible(x)
}
