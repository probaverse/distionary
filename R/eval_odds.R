#' Odds Function
#'
#' Access a distribution's odds function. The odds of an event having
#' probability `p` is `p / (1 - p)`.
#'
#' @inheritParams eval_cdf
#' @return The evaluated odds in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_pois(1)
#' eval_odds(d, at = c(1, 2, 2.5))
#' enframe_odds(d, at = 0:4)
#' @family distributional representations
#' @rdname odds
#' @export
eval_odds <- function(distribution, at) {
  p <- eval_pmf(distribution, at = at, strict = FALSE)
  p / (1 - p)
}

#' @rdname odds
#' @export
enframe_odds <- function(..., at, arg_name = ".arg", fn_prefix = "odds",
                         sep = "_") {
  enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
                  sep = sep, eval_fn = eval_odds)
}
