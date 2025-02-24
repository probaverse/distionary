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
#' @export
realise <- function(distribution, n = 1) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, 0, len = 1)
  eval_property(distribution, "realise", n)
}

#' @rdname realise
#' @export
realize <- function(distribution, n = 1) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_integerish(n, 0, len = 1)
  eval_property(distribution, "realize", n)
}
