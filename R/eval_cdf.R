#' Cumulative Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param distribution,... A distribution, or possibly multiple
#' distributions in the case of `...`.
#' @param at Vector of values to evaluate the cdf at. Must be named when using
#' in `enframe_`.
#' @param arg_name For `enframe_`, name of the column containing
#' the function arguments.
#' @param fn_prefix For `enframe_`, name of the function to
#' appear in the column(s).
#' @param sep When `enframe`'ing more than one distribution, the
#' character that will be separating the `fn_name` and the distribution name.
#' @return The evaluated cdf in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @family distributional representations
#' @examples
#' d1 <- dst_unif(0, 4)
#' d2 <- dst_pois(1.1)
#' eval_cdf(d1, at = 0:4)
#' enframe_cdf(d1, at = 0:4)
#' enframe_cdf(d1, d2, at = 0:4)
#' @rdname cdf
#' @export
eval_cdf <- function(distribution, at) UseMethod("eval_cdf")

#' @export
eval_cdf.dst <- function(distribution, at) {
  stop("Can't find a cdf for this distribution.")
}

#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
                  sep = sep, eval_fn = eval_cdf)
}
