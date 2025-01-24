parameter_space <- function(distribution) {
  distribution[["parameters"]][["space"]]
}

#' Check whether parameter values are valid
#'
#' Check whether parameter values fall within a distribution family's
#' parameter space.
#' @param distribution distribution
#' @param ... Named values to check. Evaluated in caller environment.
#' @param .stop Stop if parameters are not valid?
#' @returns Single logical indicating whether the parameter values in `...`
#' fall within the parameter space, given the values of parameters that are
#' already resolved. May return `NA` if validity cannot be determined;
#' for example, if `sigma > mu` is a restriction, testing `sigma = 4`
#' will return `NA` unless `mu` is known.
#' @examples
#' d <- distribution(.params = params(a, b, a > b, b > 0))
#' validate_parameters(d, a = 5, b = 2)
#' validate_parameters(d, b = -2)
#' validate_parameters(d, a = 20)
#' d <- set_parameters(d, b = 1)
#' validate_parameters(d, b = 2) ## TO DO -- should be FALSE.
#' validate_parameters(d, a = 1)
#' validate_parameters(d, a = 20)
#' # d <- set_parameters(d, a < 10)
#' # validate_parameters(d, a = 20)
validate_parameters <- function(distribution, ..., .stop = FALSE) {
  ## replace the following with set_parameters()? -----
  dots <- rlang::list2(...) # To do: flatten lists to accept list entries.
  already_set <- parameters(distribution)
  not_already_set <- vapply(already_set, is.null, FUN.VALUE = logical(1L))
  param_values <- append(dots, already_set[!not_already_set])
  ## END replace -----
  e <- repres_env(distribution)
  space <- parameter_space(distribution)
  valid_spaces <- vapply(
    space,
    function(expr_) {
      t <- try(
        rlang::eval_tidy(expr_, data = param_values, env = e),
        ## TO DO: consider switching all distribution expressions to quosures.
        silent = TRUE
      )
      if (inherits(t, "try-error")) t <- NA
      t
    },
    FUN.VALUE = logical(1L)
  )
  are_valid <- all(valid_spaces)
  if (!are_valid && .stop) {
    baddies <- as.character(space[!valid_spaces])
    stop(
      "Specified parameters are not valid. Violated spaces: ",
      paste0(space, collapse = ", "), "."
    )
  }
  are_valid
}
