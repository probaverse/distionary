#' Constructor functions for distribution objects
#'
#' Create an object of class `"dst"` (univariate), `"bidst"` (bivariate),
#' or `"multidst"` (multivariate).
#'
#' @param l List containing the components of a distribution object.
#' @param variable Vector of random variable types, one for each dimension.
#' Possibilities are "continuous", "discrete", or "mixed".
#' @param dimension Integer; number of variables associated with the
#' distribution.
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @note `new_distribution()` is included for legacy purposes, and is
#' identical to `new_dst()`.
#' @rdname new_distribution
new_dst <- function(l, variable, ..., class = character()) {
  structure(l, variable = variable, dimension = 1L, ...,
            class = c(class, "dst"))
}

#' @rdname new_distribution
new_bidst <- function(l, variable, ..., class = character()) {
  structure(l, variable = variable, dimension = 2L, ...,
            class = c(class, "bidst"))
}

#' @rdname new_distribution
new_multidst <- function(l, variable, dimension, ..., class = character()) {
  structure(l, variable = variable, dimension = dimension, ...,
            class = c(class, "multidst"))
}

#' @rdname new_distribution
new_distribution <- new_dst

#' Distribution Objects
#'
#' Check if an object is a distribution.
#'
#' @param object Object to be tested
#' @rdname is_distribution
#' @export
is_distribution <- function(object) {
  is_dst(object) || is_bidst(object) || is_multidst(object)
}

#' @rdname is_distribution
#' @export
is_dst <- function(object) inherits(object, "dst")

#' @rdname is_distribution
#' @export
is_bidst <- function(object) inherits(object, "bidst")

#' @rdname is_distribution
#' @export
is_multidst <- function(object) inherits(object, "multidst")


#' Make a blank distribution
#'
#' Currently, this function makes a distribution object with nothing in it. The
#' idea is that you can then set things downstream, with functions such as
#' set_cdf() and set_mean(). The idea behind this function is expected to be in
#' flux.
#'
#' @param variable Is this variable continuous, discrete, or mixed?
#' @return A distribution object with nothing in it.
#' @export
distribution <- function(variable = c("continuous", "discrete", "mixed")) {
  variable <- match.arg(variable)
  new_distribution(list(), variable = variable)
}
