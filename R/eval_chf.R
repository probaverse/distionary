#' Cumulative Hazard Function
#'
#' Access a distribution's cumulative hazard function (chf).
#'
#' @inheritParams eval_cdf
#' @return The evaluated cumulative hazard
#' in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_unif(0, 4)
#' eval_chf(d, at = 0:4)
#' enframe_chf(d, at = 0:4)
#' @family distributional representations
#' @rdname chf
#' @export
eval_chf <- function(distribution, at) {
  eval_representation(distribution, "chf", at)
}

eval_chf_from_network <- function(distribution, at) {
  if (variable(distribution) == "continuous") {
    sf <- eval_survival(distribution, at = at)
    -log(sf)
  } else {
    stop("Not valid for non-continuous distributions.")
  }
}

#' @rdname chf
#' @export
enframe_chf <- function(..., at, arg_name = ".arg", fn_prefix = "chf",
						sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_chf)
}
