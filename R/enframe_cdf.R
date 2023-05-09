#' Enframe a cumulative distribution function (CDF)
#'
#' Evaluates the CDF of the specified distributions, placing the inputs
#' and outputs in a data frame.
#'
#' @inheritParams eval_cdf
#' @param ... Distributions, or possibly a nested list of distributions,
#' inheriting the class `"dst"`, `"bidst"`, or `"multidst"`.
#' Must be of the same dimension.
#' @param arg_name Name(s) of the column(s) containing
#' the function arguments, one per dimension of the distribution. See details.
#' @param fn_prefix Name of the prefix to appear in the function
#' evaluation column(s).
#' @param sep Deprecated; no longer used.
#' @details Column names can be specified directly through
#' `arg_name` (for the input columns) and `fn_prefix` (for the distributions /
#' output columns), one name per column. If only one name is supplied, this
#' name is included as a prefix to the
#' automated naming convention, which arrives at names using the following
#' preferential order:
#'
#' 1. Given names, where applicable (as in `...` and `.l`).
#' 2. Variable names input as arguments.
#' 3. `.x` and `.y`, in the case of bivariate enframing, or `...1`, `...2`,
#'    etc. in the case of multivariate enframing.
#'
#' @return A data frame, or a tibble if the tibble package is accessible, with
#' one column per argument (one in the case of `at`; two in the case
#' of `x`, `y`), and one column per distribution input into `...`.
#' @rdname enframe_cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  send_fmls_to_enframe("uni", "cdf")
}

#' @rdname enframe_cdf
#' @export
enframe_bi_cdf <- function(..., x, y, arg_name = NULL, fn_prefix = "cdf",
                           sep = "_") {
  send_fmls_to_enframe("bi", "cdf")
}

#' @rdname enframe_cdf
#' @export
enframe_multi_cdf <- function(..., .l, arg_name = NULL, fn_prefix = "cdf",
                              sep = "_") {
  send_fmls_to_enframe("multi", "cdf")
}

