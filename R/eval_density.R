#' Probability Density Function
#'
#' Access a distribution's probability density function (pdf).
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_unif(0, 4)
#' eval_density(d, at = 0:4)
#' enframe_density(d, at = 0:4)
#' @family distributional representations
#' @rdname density
#' @export
eval_density <- function(distribution, at) {
  eval_property(distribution, "density", at)
}

#' @noRd
eval_density_from_network <- function(distribution, at) {
  stop(
    "Cannot find density function. Density must be specified ",
    "in the distribution."
  )
}

#' @rdname density
#' @export
enframe_density <- function(..., at, arg_name = ".arg", fn_prefix = "density",
                            sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_density
  )
}
