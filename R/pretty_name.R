#' Print the name of a distribution, possibly with parameters
#'
#' @param distribution Distribution object.
#' @param param_digits How many significant digits to include when displaying
#' the parameters? `0` if you don't want to display parameters.
#' @return A character containing the distribution's name, possibly
#' followed by parameters in brackets.
#' @export
pretty_name <- function(distribution, param_digits = 0) {
  if (is_empirical(distribution)) {
    name <- "empirical"
  } else {
    name <- distribution$name
  }
  low_name <- tolower(name)
  if (low_name == "frechet") name <- "Frechet"
  if (low_name == "gev") name <- "GEV"
  if (low_name == "gpd") name <- "GPD"
  if (param_digits > 0) {
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
#' @return a single character with the parameters separated by commas and
#' sandwiched by brackets.
bracket_parameters <- function(distribution, param_digits = 2) {
  p <- signif(unlist(parameters(distribution)), digits = param_digits)
  paste0("(", paste0(p, collapse = ", "), ")")
}
