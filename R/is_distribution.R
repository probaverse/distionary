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


