#' @export
print.dst <- function(x, ...) {
  nm <- x$name
  if (is.null(nm)) {
    nm <- "Unnamed distribution."
  }
  cat(nm)
  invisible(x)
}
