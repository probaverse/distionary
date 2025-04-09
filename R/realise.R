#' Generate a Sample from a Distribution
#'
#' Draw `n` independent observations from a distribution.
#'
#' @param distribution Distribution object.
#' @param n Number of observations to generate.
#' @returns Vector of independent values drawn from the inputted distribution.
#' @note `realise()` and `realize()` are aliases and do the same thing.
#' @rdname realise
#' @examples
#' d <- dst_pois(5)
#' set.seed(2)
#' realise(d, n = 10)
#' @srrstats {G2.4a} Explicit conversion to `integer` via `as.integer()`
#' becomes superfluous after checking that a number is "integerish" using the
#' checkmate package.
#' @srrstats {G2.6} distionary asserts one-dimensional input where required
#' (e.g., `dst_*()` specifications) using the checkmate package.
#' @export
realise <- function(distribution, n = 1) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, lower = 0, len = 1)
  eval_property(distribution, "realise", n)
}

#' @rdname realise
#' @export
realize <- function(distribution, n = 1) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, lower = 0, len = 1)
  eval_property(distribution, "realize", n)
}
