#' Constructor Function for "dst" Objects
#'
#' @param l Object containing the components of a distribution object.
#' @param ... Attributes to add to the object.
#' @param class If making a subclass, specify its name here.
#' @export
new_distribution <- function(l, ..., class = character()) {
  structure(l, ..., class = c(class, "dst"))
}

#' Distribution Objects
#' @param object Object to be tested
#' @rdname distribution
#' @export
is_distribution <- function(object) inherits(object, "dst")

#' @rdname distribution
#' @export
is.distribution <- is_distribution

#' Validate a distribution object
#'
#' A low-level checker that all the fields of a distribution object are in
#' place. Does not check whether the distributional representations and
#' properties are valid.
#' @inheritParams is_distribution
#' @returns The input object, invisibly.
#' @export
validate_distribution <- function(object) {
  nms <- names(object)
  if (!"name" %in% nms) {
    stop("Distribution is missing a 'name' field.")
    if (!is.null(object$name)) {
      if (length(object$name) != 0) {
        stop("Distribution name provided, but is not length 1.")
      }
    }
  }
  if (!"vtype" %in% nms) {
    stop("Distribution is missing a 'vtype' field.")
  }
  if (!"parameters" %in% nms) {
    stop("Distribution is missing a 'parameters' field.")
  }
  if (!is.environment(object[["repres_env"]])) {
    stop("Distribution needs a 'repres_env' field containing an environment.")
  }
  invisible(object)
}
