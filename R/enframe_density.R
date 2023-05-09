#' Enframe a probability density function (PDF)
#'
#' Evaluates the density of the specified distributions, placing the inputs
#' and outputs in a data frame.
#'
#' @inheritParams enframe_cdf
#' @inheritParams eval_density
#' @inherit enframe_cdf details
#' @inherit enframe_cdf return
#' @rdname enframe_density
#' @export
enframe_density <- function(..., at, arg_name = ".arg",
                            fn_prefix = "density", sep = "_", strict = TRUE) {
  send_fmls_to_enframe("uni", "density")
}

#' @rdname enframe_density
#' @export
enframe_bi_density <- function(..., x, y, arg_name = NULL,
                               fn_prefix = "density", sep = "_",
                               strict = TRUE) {
  send_fmls_to_enframe("bi", "density")
}

#' @rdname enframe_density
#' @export
enframe_multi_density <- function(..., .l, arg_name = NULL,
                                  fn_prefix = "density", sep = "_",
                                  strict = TRUE) {
  send_fmls_to_enframe("multi", "density")
}

