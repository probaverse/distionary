#' Get a distribution's representation as a function
#'
#' @param distribution Distribution to extract a representation from.
#' @param representation Character, such as `"cdf"`. In general, a suffix
#' to an `eval_` function.
#' @returns A function of the representation.
#' @examples
#' d <- dst_nbinom(10, 0.4)
#' cdf <- distionary:::representation_as_function(d, "cdf")
#' cdf(0:10)
representation_as_function <- function(distribution, representation) {
  f <- distribution[[representation]]
  if (is.null(f)) {
    f <- \(x) eval_property(distribution, representation, x)
  }
	f
}
