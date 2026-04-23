#' Odds Function
#'
#' Access a distribution's odds function. The odds of an event having
#' probability `p` is `p / (1 - p)`.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_pois(1)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' eval_odds(d, at = c(1, 2, 2.5))
#' enframe_odds(d, at = 0:4)
#' @family distributional representations
#' @rdname odds
#' @export
eval_odds <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "odds", at)
}

#' @noRd
eval_odds_from_network <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  p <- eval_pmf(distribution, at = at)
  p / (1 - p)
}

#' @rdname odds
#' @export
enframe_odds <- function(..., at, arg_name = ".arg", fn_prefix = "odds",
                         sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_odds
  )
}
