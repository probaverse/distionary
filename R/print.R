#' @export
print.dst <- function(x, ...) {
  nm <- attributes(x)[["name"]]
  vtype <- attributes(x)[["vtype"]]
  param <- parameters(x)
  # Family name
  if (is.null(nm)) {
    cat("Unnamed distribution")
  } else {
    cat(nm, "distribution")
  }
  # Variable type, and the variables if there are several
  if (!is.null(vtype)) {
    vars <- variables(x)
    if (is.null(vars)) {
      cat(paste0(" (", vtype, ")", collapse = ""), "\n")
    } else {
      cat(" (", vtype, "; ", paste(vars, collapse = ", "), ")\n", sep = "")
    }
  }
  # Parameters
  if (!is.null(param) && all(!is.na(param))) {
    all_numeric <- all(vapply(
      param,
      function(x) is.numeric(x) && length(x) == 1,
      FUN.VALUE = logical(1)
    ))
    if (all_numeric) {
      param <- unlist(param)
    }
    if (is.list(param) && is.data.frame(param[["outcomes"]])) {
      param <- cbind(param[["outcomes"]], .prob = param[["probs"]])
      param <- convert_dataframe_to_tibble(param)
    } else if (pretty_name(x) == "Finite") {
      param <- as.data.frame(param)
      param <- convert_dataframe_to_tibble(param)
    }
    cat("--Parameters--\n")
    print(param, ...)
  }
  invisible(x)
}
