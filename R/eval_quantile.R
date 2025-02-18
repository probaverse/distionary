#' Distribution Quantiles
#'
#' Access a distribution's quantiles.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_unif(0, 4)
#' eval_quantile(d, at = 1:9 / 10)
#' enframe_quantile(d, at = 1:9 / 10)
#' @family distributional representations
#' @details When a quantile function does not exist, an algorithm is
#' deployed that calculates the left inverse of the CDF. This algorithm
#' works by progressively cutting the specified range in half, moving
#' into the left or right half depending on where the solution is.
#' The algorithm is not currently fast and is subject to improvement,
#' and is a simple idea that has been passed around on the internet
#' here and there. Tolerance is less than 1e-9, unless the maximum
#' number of iterations (200) is reached.
#' @srrstats {G1.1} *Statistical Software should document whether
#' the algorithm(s) it implements are:* - *The first implementation of
#' a novel algorithm*; or - *The first implementation within **R** of
#' an algorithm which has previously been implemented in other languages
#' or contexts*; or - *An improvement on other implementations of
#' similar algorithms in **R***.
#' Done, the algorithm has no specific reference.
#' @srrstats {PD3.3} *Return objects which include values generated
#' from optimisation algorithms should include information on
#' optimisation algorithm and performance, minimally including the name
#' of the algorithm used, the convergence tolerance, and the number of
#' iterations.*
#' - This is specified.
#' @rdname quantile
#' @export
eval_quantile <- function(distribution, at) {
  eval_property(distribution, "quantile", at)
}

#' @rdname quantile
#' @export
enframe_quantile <- function(
    ..., at, arg_name = ".arg", fn_prefix = "quantile",
    sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_quantile
  )
}
