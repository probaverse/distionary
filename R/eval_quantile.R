#' Distribution Quantiles
#'
#' Access a distribution's quantiles.
#'
#' @inheritParams eval_cdf
#' @return The evaluated quantiles in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_unif(0, 4)
#' eval_quantile(d, at = 1:9 / 10)
#' enframe_quantile(d, at = 1:9 / 10)
#' @family distributional representations
#' @rdname quantile
#' @export
eval_quantile <- function(distribution, at) {
  eval_property(distribution, "quantile", at)
}

#' @rdname quantile
#' @export
enframe_quantile <- function(..., at, arg_name = ".arg", fn_prefix = "quantile",
							 sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_quantile)
}
