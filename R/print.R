#' @export
print.dst <- function(x, ...) {
  nm <- attributes(x)[["name"]]
  vtype <- attributes(x)[["vtype"]]
  param <- parameters(x)
  if (is.null(nm)) {
    cat("Unnamed distribution")
  } else {
    cat(nm, "distribution")
  }
  if (!is.null(vtype)) {
    cat(paste0(" (", vtype, ")", collapse = ""))
  }
  if (!is.null(param) && all(!is.na(param))) {
    all_numeric <- all(vapply(
      param, \(x) is.numeric(x) && length(x) == 1, FUN.VALUE = logical(1)
    ))
    if (all_numeric) {
      param <- unlist(param)
    }
    cat("\n--Parameters--\n")
    print(param)
  }
  invisible(x)
}
