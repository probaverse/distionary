#' Return Level Function
#'
#' Compute return levels (quantiles) from a distribution by inputting
#' return periods. The return periods correspond to events that are
#' _exceedances_ of a quantile, not non-exceedances.
#'
#' @param at Vector of return periods >=1.
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @details This function is simply the quantile
#' function evaluated at `1 - 1 / at`.
#' @examples
#' d <- dst_gp(24, 0.3)
#' eval_return(d, at = c(2, 25, 100, 200))
#' @family distributional representations
#' @rdname return
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
#' @export
eval_return <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "return", at)
}

#' @noRd
eval_return_from_network <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_quantile(distribution, at = 1 - 1 / at)
}

#' @rdname return
#' @export
enframe_return <- function(..., at, arg_name = ".arg", fn_prefix = "return",
                           sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_return
  )
}
