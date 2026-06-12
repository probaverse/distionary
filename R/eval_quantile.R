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
#' deployed that calculates the left inverse of the CDF by bisection:
#' an interval known to contain the solution is progressively cut in
#' half, moving into whichever half still contains it. The whole vector
#' of requested probabilities is solved together (one vectorized CDF
#' evaluation per step rather than one per probability), so evaluating
#' many quantiles at once is considerably faster than one at a time.
#'
#' For a distribution with a structured support (see [support()]), the
#' algorithm is aware of where the atoms (discrete mass points) are. A
#' probability that lands inside an atom's jump in the CDF is returned
#' as that atom exactly, rather than approximately, and the boundary
#' quantiles (at probability 0 and 1) are read straight from the
#' support. Tolerance is roughly 1e-9 in the quantile value, unless the
#' maximum number of iterations (200) is reached.
#' @rdname quantile
#' @export
eval_quantile <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
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
