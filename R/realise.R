#' Generate a Sample from a Distribution
#'
#' Draw `n` independent observations from a distribution.
#'
#' @param distribution Distribution.
#' @param n Number of observations to generate
#' @return Vector of independent values drawn from the inputted distribution
#' @note `realise()`, `realize()`, and `observe()` all do the same thing.
#' Although the language `observe()` is more common, it's only an alias here,
#' because this function name is common amongst other packages.
#' @rdname realise
#' @export
realise <- function(distribution, n = 1) UseMethod("realise")

#' @export
realise.dst <- function(distribution, n = 1) {
  u <- stats::runif(n)
  return(eval_quantile(distribution, at = u))
}

#' @rdname realise
#' @export
realize <- function(distribution, n = 1) {
	realise(distribution, n = n)
}

#' @rdname realise
#' @export
observe <- function(distribution, n = 1) {
	realise(distribution, n = n)
}
