#' Get a distribution's representation as a function
#'
#' @param distribution Distribution to extract a representation from.
#' @param representation Character, such as `"cdf"`. In general, a suffix
#' to an `eval_` function. Vector of length 1.
#' @param variant Which variant of the representation to extract, as a named
#' list of departures from the canonical variant; see [eval_property()].
#' @returns A function of the representation.
#' @noRd
representation_as_function <- function(
  distribution,
  representation,
  variant = list()
) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_character(representation, len = 1)
  variant <- check_variant(variant)
  stored <- distribution[[representation]]
  f <- NULL
  if (!is.null(stored)) {
    f <- representation_function(stored, variant)
  }
  if (is.null(f)) {
    f <- function(x) {
      eval_property(distribution, representation, x, variant = variant)
    }
  }
  f
}
