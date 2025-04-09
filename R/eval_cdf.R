#' Cumulative Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param distribution,... A distribution, or possibly multiple
#' distributions in the case of `...`.
#' @param at Vector of values to evaluate the representation at.
#' @param arg_name For `enframe_`, name of the column containing
#' the function arguments. Length 1 character vector.
#' @param fn_prefix For `enframe_`, name of the function to
#' appear in the column(s). Length 1 character vector.
#' @param sep When `enframe`'ing more than one distribution, the
#' character that will be separating the `fn_name` and the distribution name.
#' Length 1 character vector.
#' @returns The evaluated representation in vector form (for `eval_`)
#' with length matching the length of `at`, and data frame
#' or tibble form (for `enframe_`) with number of rows matching the
#' length of `at`. The `at` input occupies the first column,
#' named `.arg` by default, or the specification in `arg_name`;
#' the evaluated representations for each distribution in `...`
#' go in the subsequent columns (one column per distribution). For a
#' single distribution, this column is named according to the
#' representation by default (cdf, survival, quantile, etc.),
#' or the value in `fn_prefix`. For multiple distributions, unnamed
#' distributions are auto-named, and columns are named
#' `<fn_prefix><sep><distribution_name>` (e.g., `cdf_distribution1`).
#' @family distributional representations
#' @examples
#' d1 <- dst_unif(0, 4)
#' d2 <- dst_pois(1.1)
#' eval_cdf(d1, at = 0:4)
#' enframe_cdf(d1, at = 0:4)
#' enframe_cdf(d1, d2, at = 0:4)
#' enframe_cdf(model1 = d1, model2 = d2, at = 0:4)
#' enframe_cdf(
#'   model1 = d1, model2 = d2, at = 0:4, arg_name = "value"
#' )
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user
#' wants this behaviour, it should be explicitly specified in their code,
#' because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#' @srrstats {G2.16} This version of distionary does force the propagation of
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user
#' specification for length-stability, also because `Inf` and `-Inf` are
#' expected in some cases (e.g., the support of any Normal distribution).
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
#' @rdname cdf
#' @export
eval_cdf <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "cdf", at)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_cdf
  )
}
