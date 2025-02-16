#' Return Level Function
#'
#' Compute return levels (quantiles) from a distribution by inputting
#' return periods. The return periods correspond to events that are
#' _exceedances_ of a quantile, not non-exceedances.
#'
#' @param at Vector of return periods >=1.
#' @inheritParams eval_cdf
#' @returns The return levels associated with the specified return periods
#' in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`). This function is simply the quantile
#' function evaluated at `1 - 1 / at`.
#' @examples
#' d <- dst_gpd(0, 1)
#' eval_return(d, at = c(2, 25, 100, 200))
#' @family distributional representations
#' @rdname return
#' @export
eval_return <- function(distribution, at) {
  eval_property(distribution, "return", at)
}

eval_return_from_network <- function(distribution, at) {
  eval_quantile(distribution, at = 1 - 1 / at)
}

#' @rdname return
#' @export
enframe_return <- function(..., at, arg_name = ".arg", fn_prefix = "return",
                           sep = "_") {
  enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
                  sep = sep, eval_fn = eval_return)
}
