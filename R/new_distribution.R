#' Constructor Function for "dst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_distribution <- function(l, ..., class = character()) {
  structure(l, ..., class = append(class, "dst"))
}

#' Distribution Objects
#' @param object Object to be tested
#' @rdname distribution
#' @export
is_distribution <- function(object) inherits(object, "dst")

#' @rdname distribution
#' @export
is.distribution <- function(object) inherits(object, "dst")
