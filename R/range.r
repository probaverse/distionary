#' Range of Distribution
#'
#' Range returns a vector of length two, with the minimum and maximum
#' values of the (support of the) distribution.
#'
#' @param distribution Distribution to compute range from.
#' @param ... Not used; vestige of the `base::range()` S3 generic.
#' @details
#' The range is read from the distribution's support (see [support()]), which
#' is where a distribution says what values it reaches. In this it behaves like
#' [vtype()]: derived, not declared.
#'
#' `range` is therefore not one of the property names distionary recognises. A
#' `range` entry given to [distribution()] is kept, like any other name it does
#' not know, but nothing consults it --- this function included.
#'
#' The Null distribution has no support, and its range is `NA`.
#' @returns Vector of length two, containing the minimum and maximum
#' values of a distribution.
#' @examples
#' a <- dst_gp(1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' range(a)
#' range(b)
#' range(c)
#' @rdname range
#' @export
range.dst <- function(distribution, ...) {
  checkmate::assert_class(distribution, "dst")
  dots <- rlang::enexprs(...)
  dots[["na.rm"]] <- NULL
  if (length(dots) > 0) {
    stop(
      "`range()` is expecting no arguments in `...`. ",
      "Did you accidentally misspell 'distribution'?"
    )
  }
  s <- support(distribution)
  if (is.null(s)) {
    # Only the Null distribution has no support, and it reaches nothing.
    return(c(NA_real_, NA_real_))
  }
  support_hull(s)
}

#' @description
#' The `support` method gives the smallest and largest values the support
#' reaches --- its two outermost points, taking the atoms and the continuous
#' intervals together. Gaps in between are not represented. For the empty
#' support, both are `NA`.
#' @param support A support object.
#' @examples
#' range(continuous(c(0, 1), c(3, 4)))
#' range(mixed(atoms = -1, continuous = c(0, Inf)))
#' range(empty_support())
#' @rdname range
#' @export
range.support <- function(support, ...) {
  dots <- rlang::enexprs(...)
  dots[["na.rm"]] <- NULL
  if (length(dots) > 0) {
    stop("`range()` is expecting no arguments in `...`.")
  }
  support_hull(support)
}
