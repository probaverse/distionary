#' Constructor Function for "dst" Objects
#'
#' @param l Object containing the components of a distribution object.
#' @param vtype Type of random variable. Common options
#' include "continuous", "discrete", or "mixed", but remains open for
#' extensibility.
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
  if (!"params" %in% nms) {
    stop("Distribution is missing a 'params' field.")
  }
  if (!is.environment(object[["repres_env"]])) {
    stop("Distribution needs a 'repres_env' field containing an environment.")
  }
  invisible()
}
