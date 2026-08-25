#' Distribution Quantiles
#'
#' Access a distribution's quantiles.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param side Which inverse of the cdf to take: `"left"`, the usual quantile
#' function, is the smallest `x` with `cdf(x) >= at`; `"right"` is the
#' smallest `x` with `cdf(x) > at`.
#'
#' The two differ only where the cdf is flat at level `at` over a stretch of
#' positive length --- a gap between atoms, or a region the distribution
#' places no probability in. The left inverse gives the start of that stretch,
#' the right inverse its end. Where no such stretch exists, including inside
#' an atom's jump, both give the same answer.
#'
#' `side` does not apply at `at = 0` and `at = 1`, which are the ends of the
#' support either way; see the details.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_quantile(d, at = 1:9 / 10)
#' enframe_quantile(d, at = 1:9 / 10)
#'
#' # Between the atoms of a Poisson, the two inverses part company.
#' p <- dst_pois(5)
#' eval_quantile(p, at = eval_cdf(p, at = 3))
#' eval_quantile(p, at = eval_cdf(p, at = 3), side = "right")
#' @family distributional representations
#' @details
#' The 0- and 1-quantiles are the ends of the distribution's support: the
#' 0-quantile is its lower end and the 1-quantile its upper end. They are
#' read from the support (see [support()]) rather than computed, so an
#' unbounded distribution gives `-Inf` and `Inf` instead of a large finite
#' number found by searching into the tail. `side` has no say there: each
#' inverse is degenerate at one of the two ends --- the left inverse of 0 is
#' `-Inf` for every distribution, the right inverse of 1 is `Inf` for every
#' distribution --- so a convention overrides both to put the answers on the
#' support.
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
#'
#' A distribution's own quantile function is taken to be the left inverse. A
#' distribution that can provide the right inverse too can say so with
#' [variants()]; otherwise it is found by inverting the cdf, as above.
#' @rdname quantile
#' @export
eval_quantile <- function(
  distribution,
  at,
  ...,
  side = c("left", "right")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  side <- match_variant(side, "side", "quantile")
  wanted <- variant(side = side, .entry = "quantile")
  s <- support(distribution)
  if (is.null(s)) {
    # Only the Null distribution has no support, and it brings its own
    # quantile function. It answers `NA` to every question asked of it,
    # which is as true of one inverse as of the other, so `side` is spent
    # here rather than sending a variant off to be derived from a
    # distribution that has nothing to derive it from.
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
    out[rest] <- eval_property(
      distribution,
      "quantile",
      at[rest],
      variant = wanted
    )
  }
  out
}

#' @rdname quantile
#' @export
enframe_quantile <- function(
  ...,
  at,
  side = "left",
  arg_name = ".arg",
  fn_prefix = "quantile",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_quantile,
    fn_args = list(side = side)
  )
}
