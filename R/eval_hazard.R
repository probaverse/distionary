#' Hazard Function
#'
#' Access a distribution's hazard function.
#'
#' @inheritParams eval_cdf
#' @return The evaluated hazard in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_unif(0, 4)
#' eval_hazard(d, at = 0:4)
#' enframe_hazard(d, at = 0:4)
#' @family distributional representations
#' @rdname hazard
#' @export
eval_hazard <- function(distribution, at) {
  eval_representation(distribution, "odds", at)
}

eval_hazard_from_network <- function(distribution, at) {
  if (variable(distribution) != "continuous") {
    stop("Hazard function requires a continuous distribution.")
  }
  sf <- eval_survival(distribution, at)
  pdf <- eval_density(distribution, at)
  pdf / sf
}

#' @rdname hazard
#' @export
enframe_hazard <- function(..., at, arg_name = ".arg", fn_prefix = "hazard",
						   sep = "_") {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_hazard)
}
