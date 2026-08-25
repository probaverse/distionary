#' A Distribution has Length 1
#'
#' A distribution object is one distribution, so `length()` gives 1 and
#' `is.na()` gives a single logical. `as.list()` wraps the distribution
#' in a list of one.
#'
#' @param x A distribution object.
#' @param ... Not used.
#' @details
#' A distribution is built out of a list of its properties --- a CDF, a
#' density, a mean --- and without these methods base R reports on that list
#' rather than on the distribution. `length()` counted the properties and
#' `is.na()` tested each one, so `dst_norm(0, 1)` answered with eleven
#' `FALSE`s. Neither answer was about the distribution.
#'
#' `is.na()` is `TRUE` for the Null distribution ([dst_null()]) and `FALSE`
#' for every other. The Null distribution is the missing value of the
#' distribution world, so it is the one that `is.na()` finds.
#'
#' Note that the properties are still reachable, and are still what the
#' object is made of: `x[["cdf"]]` and `names(x)` are unchanged, and
#' [eval_property()] is the supported way to get at them. Only the questions
#' asked of the distribution *as a whole* now answer about the whole.
#'
#' To hold several distributions, put them in a list; in a data frame, that
#' is a list-column. A distribution does not have length beyond one.
#' @returns For `length()`, the number 1. For `is.na()`, a single logical.
#' For `as.list()`, a list containing the one distribution.
#' @examples
#' d <- dst_norm(0, 1)
#' length(d)
#' is.na(d)
#'
#' # The Null distribution is the missing one.
#' is.na(dst_null())
#'
#' # Several distributions go in a list.
#' ds <- list(dst_norm(0, 1), dst_null(), dst_pois(3))
#' vapply(ds, is.na, logical(1))
#' @name scalar
NULL

#' @rdname scalar
#' @export
length.dst <- function(x) {
  1L
}

#' @rdname scalar
#' @export
is.na.dst <- function(x) {
  # `dst_null()` marks itself with a subclass, so that a distribution the
  # user happens to name "Null" is not mistaken for the missing one.
  inherits(x, "null_dst")
}

#' @rdname scalar
#' @export
as.list.dst <- function(x, ...) {
  # `as.list()` consults `length()`, so without this it would hand back a
  # one-element list still carrying the names of all the properties.
  rlang::check_dots_empty()
  list(x)
}
