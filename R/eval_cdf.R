#' Cumulative Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param distribution,... A distribution, or possibly multiple
#' distributions in the case of `...`.
#' @param arg_name For `enframe_`, name(s) of the column containing
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
eval_cdf <- function(distribution, ...) UseMethod("eval_cdf")

#' @param at Vector of values to evaluate the univariate cdf at.
#' @export
eval_cdf.dst <- function(distribution, at, ...) {
  stop("Can't find a cdf for this distribution.")
}

#' @param x,y Vectors of values to evaluate the bivariate cdf at.
#' @export
eval_cdf.bi_dst <- function(distribution, x, y, ...) {
  stop("Can't find a cdf for this distribution.")
}

#' @param .l List of vectors of values to evaluate the multivariate cdf at.
#' @export
eval_cdf.multi_dst <- function(distribution, .l, ...) {
  stop("Can't find a cdf for this distribution.")
}

#' @rdname cdf
#' @export
enframe_cdf <- function(...) UseMethod("enframe_cdf")

#' @rdname cdf
#' @export
enframe_cdf.dst <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                            sep = "_") {
  enframe_univariate(..., at = at,
                     arg_name = arg_name, fn_prefix = fn_prefix,
                     sep = sep, eval_fn = eval_cdf)
}

#' @rdname cdf
#' @export
enframe_cdf.bi_dst <- function(..., x, y, arg_name = ".arg", fn_prefix = "cdf",
                            sep = "_") {
  enframe_bivariate(..., x = x, y = y,
                    arg_name = arg_name, fn_prefix = fn_prefix,
                    sep = sep, eval_fn = eval_cdf)
}

#' @rdname cdf
#' @export
enframe_cdf.multi_dst <- function(..., .l, arg_name = ".arg", fn_prefix = "cdf",
                               sep = "_") {
  enframe_multivariate(..., .l,
                       arg_name = arg_name, fn_prefix = fn_prefix,
                       sep = sep, eval_fn = eval_cdf)
}
