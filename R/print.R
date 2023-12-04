#' @export
print.dst <- function(x, ...) {
  print(class(x))
  cat("\n", names(x[1]), ":\n")
  print(x[[1]])
}
