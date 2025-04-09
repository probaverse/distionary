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
#' a <- dst_gpd(1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' range(a)
#' range(b)
#' range(c)
#' @rdname range
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
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
