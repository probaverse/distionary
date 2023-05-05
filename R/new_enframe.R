#' Make a new `enframe_` function
#'
#' For a given distributional representation, these functions create
#' an `enframe_` function for that representation.
#'
#' @param repres String; name of the representation.
#' @return A function that evaluates the specified representation of inputted
#' distributions, and places the results in a data frame.
#'
#' - The `_univariate` flavour provides the `at` argument to place a single
#' vector to evaluate the representation, as in `enframe_cdf()`.
#' - The `_bivariate` flavour provides the `x` and `y` arguments to hold
#' vectors to evaluate the representation, as in `enframe_bicdf()`.
#' - The `_multivariate` flavour provides the `.l` argument to hold a list of
#' vectors to evaluate the representation, as in `enframe_multicdf()`.
#'
#' @rdname new_enframe
#' @export
new_enframe_univariate <- function(repres) {
  function(..., at, arg_name = ".arg", fn_prefix = repres, sep = "_") {
    enframe_univariate(..., at = at, arg_name = ".arg", fn_prefix = repres,
                       sep = "_", eval_fn = str_c("eval_", repres))
  }
}

#' @rdname new_enframe
new_enframe_bivariate <- function(repres) {
  function(..., x, y, arg_name = NULL, fn_prefix = repres, sep = "_") {
    enframe_bivariate(..., x = x, y = y, arg_name = ".arg", fn_prefix = repres,
                      sep = "_", eval_fn = str_c("eval_", repres))
  }
}

#' @rdname new_enframe
new_enframe_multivariate <- function(repres) {
  function(..., .l, fn_prefix = repres, sep = "_") {
    enframe_multivariate(..., .l = .l, fn_prefix = repres,
                         sep = "_", eval_fn = str_c("eval_", repres))
  }
}
