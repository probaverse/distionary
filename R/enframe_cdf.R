#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  send_fmls_to_enframe("uni", "cdf")
}

#' @rdname cdf
#' @export
enframe_bi_cdf <- function(..., x, y, arg_name = NULL, fn_prefix = "cdf",
                           sep = "_") {
  send_fmls_to_enframe("bi", "cdf")
}

#' @rdname cdf
#' @export
enframe_multi_cdf <- function(..., .l, arg_name = NULL, fn_prefix = "cdf",
                              sep = "_") {
  send_fmls_to_enframe("multi", "cdf")
}

