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
#' @details
#' The 0- and 1-quantiles are the ends of the distribution's support: the
#' 0-quantile is its lower end and the 1-quantile its upper end. They are
#' read from the support (see [support()]) rather than computed, so an
#' unbounded distribution gives `-Inf` and `Inf` instead of a large finite
#' number found by searching into the tail.
#'
#' When a quantile function does not exist, the remaining probabilities are
#' found by inverting the CDF by bisection: an interval known to contain the
#' solution is progressively cut in half, moving into whichever half still
#' contains it. The whole vector is solved together --- one vectorized CDF
#' evaluation per step rather than one per probability --- so evaluating many
#' quantiles at once is considerably faster than one at a time. Because the
#' support says where the atoms (discrete mass points) are, a probability
#' landing inside an atom's jump in the CDF is returned as that atom exactly,
#' rather than approximately. Tolerance is roughly 1e-9 in the quantile value,
#' unless the maximum number of iterations (200) is reached.
#' @rdname quantile
#' @export
eval_quantile <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  s <- support(distribution)
  if (is.null(s)) {
    # Only the Null distribution has no support, and it brings its own
    # quantile function.
    return(eval_property(distribution, "quantile", at))
  }
  # The boundary quantiles are a property of the support, not something to
  # solve for, so they are settled here and never reach an algorithm. Read the
  # support directly rather than calling `range()`, which is itself derived
  # from the quantiles for a distribution that has no support.
  hull <- support_hull(s)
  out <- rep(NA_real_, length(at))
  is_zero <- !is.na(at) & at == 0
  is_one <- !is.na(at) & at == 1
  out[is_zero] <- hull[[1L]]
  out[is_one] <- hull[[2L]]
  # Everything else --- including `NA` and anything outside [0, 1] --- goes to
  # the quantile function or the network, which handle them as they always did.
  rest <- !is_zero & !is_one
  if (any(rest)) {
    out[rest] <- eval_property(distribution, "quantile", at[rest])
  }
  out
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
