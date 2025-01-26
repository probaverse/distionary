#' Range of Distribution
#'
#' Range returns a 2 index vector with the 0th index
#' containing the minimum value, and the 1st index containing the maximum value
#' for a given distribution.
#'
#' @param distribution Single distribution to compute range from.
#' @param ... Not used; vestige of the `base::range()` S3 generic.
#' @details If there are no methods for the distribution's class,
#' the range is calculated
#' using `eval_quantile()` at 0 and at 1.
#' @examples
#' a <- dst_gpd(0, 1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' range(a)
#' range(b)
#' range(c)
#' @rdname range
#' @export
range.dst <- function(distribution, ...) {
  dots <- rlang::enexprs(...)
  dots[["na.rm"]] <- NULL
  if (length(dots) > 0) {
    stop(
      "`range()` is expecting no arguments in `...`. ",
      "Did you accidentally misspell 'distribution'?"
    )
  }
  eval_representation(distribution, "range")
}

eval_range_from_network <- function(distribution) {
  eval_quantile(distribution, at = 0:1)
}
