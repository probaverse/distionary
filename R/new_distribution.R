#' Constructor Function for Distribution Objects
#'
#' Low level constructor function for creating distribution objects.
#' Consider the user-facing constructor function, `distribution()`.
#'
#' @param l List containing the components of a distribution object.
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @returns The original list `l` with the specified attributes.
#' @examples
#' new_distribution(list(mean = 3))
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G5.3} Functions that are expected to return objects containing no
#' missing (`NA`) or undefined (`NaN`, `Inf`) values are tested either
#' implicitly (e.g., `is_distribution()` implicitly checks non-NA value) or
#' explicitly (e.g., `pretty_name()` is never NA). --> Copied to both files.
#' @export
new_distribution <- function(l, ..., class = character()) {
  checkmate::assert_list(l)
  checkmate::assert_character(class)
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
