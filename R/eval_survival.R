#' Survival Function
#'
#' Access a distribution's survival function.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_unif(0, 4)
#' eval_survival(d, at = 0:4)
#' enframe_survival(d, at = 0:4)
#' @family distributional representations
#' @rdname survival
#' @export
eval_survival <- function(distribution, at) {
  eval_property(distribution, "survival", at)
}

#' @noRd
eval_survival_from_network <- function(distribution, at) {
  1 - eval_cdf(distribution, at = at)
}

#' @rdname survival
#' @export
enframe_survival <- function(..., at, arg_name = ".arg", fn_prefix = "survival",
                             sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_survival
  )
}
