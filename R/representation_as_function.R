#' Get a distribution's representation as a function
#'
#' @param distribution Distribution to extract a representation from.
#' @param representation Character, such as `"cdf"`. In general, a suffix
#' to an `eval_` function. Vector of length 1.
#' @returns A function of the representation.
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @noRd
representation_as_function <- function(distribution, representation) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_character(representation, len = 1)
  f <- distribution[[representation]]
  if (is.null(f)) {
    f <- \(x) eval_property(distribution, representation, x)
  }
  f
}
