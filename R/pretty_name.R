#' Distribution name
#'
#' Print the name of a distribution, possibly with parameters.
#'
#' @param distribution Distribution object.
#' @param param_digits How many significant digits to include when displaying
#' the parameters? `0` if you don't want to display parameters. Length 1
#' vector.
#' @returns A character containing the distribution's name, possibly
#' followed by parameters in brackets.
#' @examples
#' d <- dst_norm(0.3552, 1.1453)
#' pretty_name(d)
#' pretty_name(d, 2)
#' @srrstats {G5.3} Functions that are expected to return objects containing no
#' missing (`NA`) or undefined (`NaN`, `Inf`) values are tested either
#' implicitly (e.g., `is_distribution()` implicitly checks non-NA value) or
#' explicitly (e.g., `pretty_name()` is never NA). --> Copied to both files.
#' @export
pretty_name <- function(distribution, param_digits = 0) {
  checkmate::check_class(distribution, "dst")
  checkmate::check_integerish(param_digits, 0, len = 1)
  name <- attributes(distribution)$name
  if (is.null(name)) {
    return("Unnamed distribution")
  }
  low_name <- tolower(name)
  if (low_name == "frechet") {
    name <- "Fr\xE9chet"
    Encoding(name) <- "latin1"
    name <- iconv(
      name,
      "latin1",
      "UTF-8"
    )
  }
  if (low_name == "gev") name <- "GEV"
  if (low_name == "gp") name <- "GP"
  if (param_digits > 0 && !is.null(parameters(distribution))) {
    brackets <- bracket_parameters(distribution, param_digits = param_digits)
    name <- paste0(name, brackets)
  }
  name
}

#' Present parameters using bracket notation
#'
#' For distributions that have parameters, this function places those
#' parameters in bracket notation, such as (0, 1) for a Normal distribution
#' with a mean of 0 and variance of 1.
#'
#' @param distribution A single distplyr distribution for which the
#' `parameters()` function can be applied.
#' @param param_digits Number of significant digits to include in the
#' output.
#' @returns a single character with the parameters separated by commas and
#' sandwiched by brackets.
#' @noRd
bracket_parameters <- function(distribution, param_digits = 2) {
  p <- signif(unlist(parameters(distribution)), digits = param_digits)
  paste0("(", paste0(p, collapse = ", "), ")")
}
