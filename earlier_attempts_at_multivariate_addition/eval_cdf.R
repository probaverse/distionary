#' Cumulative Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param distribution,... A distribution, or possibly multiple
#' distributions in the case of `...`.
#' @param at Vector of values to evaluate the function at.
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
eval_cdf <- function(distribution, at) UseMethod("eval_cdf")

#' @export
eval_cdf.dst <- function(distribution, at) {
  stop("Can't find a cdf for this distribution.")
}

#' Multivariate Cumulative Distribution Function
#'
#' Access a multivariate distribution's cumulative distribution function (cdf).
#' Bivariate function is provided for convenience.
#'
#' @param x,y Vectors of values to evaluate the bivariate cdf at.
#' @param .l List of vectors of values to evaluate the multivariate cdf at.
#' @param ... Other arguments, currently not used, for extension.
#' @return The evaluated cdf in vector form.
#' @family distributional representations
#' @rdname multicdf
#' @export
eval_bicdf <- function(distribution, x, y, ...) {
  .l <- vctrs::vec_recycle_common(x, y)
  eval_multicdf(distribution, .l, ...)
}
#' @rdname multicdf
#' @export
eval_multicdf <- function(distribution, .l, ...) UseMethod("eval_multicdf")

#' @export
eval_multicdf.multi_dst <- function(distribution, .l, ...) {
  stop("Can't find a cdf for this distribution.")
}



#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  at_list <- rlang::set_names(list(at), arg_name)
  enframe_general(..., .l = at_list, fn_prefix = fn_prefix, sep = sep,
                  eval_fn = eval_cdf)
}

#' @export
enframe_bicdf <- function(..., x, y, arg_name = NULL, fn_prefix = "cdf",
                          sep = "_") {
  xy <- rlang::quos(x = x, y = y)
  names(xy) <- arg_name
  xy2 <- vec_recycle_common_and_rename(!!!xy)
  enframe_general(..., .l = xy2, fn_prefix = fn_prefix, sep = sep,
                  eval_fn = eval_bicdf)
}

#' @export
enframe_multicdf <- function(..., .l, fn_prefix = "cdf", sep = "_") {
  .l2 <- vec_recycle_common_and_rename(!!!.l)
  enframe_general(..., .l = .l2, fn_prefix = fn_prefix, sep = sep,
                  eval_fn = eval_multicdf)
}



