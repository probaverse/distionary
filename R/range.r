#' Range of Distribution
#'
#' Range returns a vector of length two, with the minimum and maximum
#' values of the (support of the) distribution.
#'
#' @param distribution Distribution to compute range from.
#' @param ... Not used; vestige of the `base::range()` S3 generic.
#' @details If there are no methods for the distribution's class,
#' the range is calculated
#' using `eval_quantile()` at 0 and at 1.
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
  eval_property(distribution, "range")
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
