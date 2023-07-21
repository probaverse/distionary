#' Make a new `enframe_` function
#'
#' For a given distributional representation, these functions create
#' an `enframe_` function for that representation.
#'
#' @param repres String; name of the representation, such as `"density"`
#' or `"cdf"`.
#' @return A function that evaluates the specified representation of inputted
#' distributions, and places the results in a data frame.
#'
#' - The `_uni` flavour provides the `at` argument to place a single
#' vector to evaluate the representation, as in `enframe_cdf()`.
#' - The `_bi` flavour provides the `x` and `y` arguments to hold
#' vectors to evaluate the representation, as in `enframe_bi_cdf()`.
#' - The `_multi` flavour provides the `.l` argument to hold a list of
#' vectors to evaluate the representation, as in `enframe_multi_cdf()`.
#'
#' @rdname new_enframe
#' @export
new_enframe_uni <- function(repres) {
  if (repres %in% c("density", "pmf")) {
    function(..., at, arg_name = ".arg", fn_prefix = repres, sep = "_",
             strict = TRUE) {
      send_fmls_to_enframe("uni", repres)
    }
  } else {
    function(..., at, arg_name = ".arg", fn_prefix = repres, sep = "_") {
      send_fmls_to_enframe("uni", repres)
    }
  }
}

#' @rdname new_enframe
new_enframe_bi <- function(repres) {
  if (repres %in% c("density", "pmf")) {
    function(..., x, y, arg_name = NULL, fn_prefix = repres, sep = "_",
             strict = TRUE) {
      send_fmls_to_enframe("bi", repres)
    }
  } else {
    function(..., x, y, arg_name = NULL, fn_prefix = repres, sep = "_") {
      send_fmls_to_enframe("bi", repres)
    }
  }
}

#' @rdname new_enframe
new_enframe_multi <- function(repres) {
  if (repres %in% c("density", "pmf")) {
    function(..., .l, arg_name = NULL, fn_prefix = "density", sep = "_",
             strict = TRUE) {
      send_fmls_to_enframe("multi", repres)
    }
  } else {
    function(..., .l, arg_name = NULL, fn_prefix = "density", sep = "_") {
      send_fmls_to_enframe("multi", repres)
    }
  }
}
