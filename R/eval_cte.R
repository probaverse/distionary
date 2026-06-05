#' Conditional Tail Expectation
#'
#' Access a distribution's (upper) conditional tail expectation.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @details The conditional tail expectation at a threshold `x` is the mean
#' outcome given that the outcome exceeds `x`,
#'
#'   `CTE(x) = E[X | X > x] = x + phi(x) / S(x)`,
#'
#' where `S` is the survival function and `phi(x) = E[(X - x)^+]` is the upper
#' partial moment. It is also known as the expected shortfall, tail
#' value-at-risk, or conditional value-at-risk, although those names are often
#' indexed by a probability level rather than a threshold. The argument `at`
#' here is a threshold on the scale of the outcome, as in [eval_survival()].
#'
#' The conditional tail expectation characterises a distribution with a finite
#' first moment, so a distribution can be specified through its conditional
#' tail expectation alone, from which the cdf and the rest of the network are
#' recovered by mean-residual-life inversion. When it is not intrinsically
#' defined, it is computed from the survival function and the mean by
#' integrating the survival function numerically. As `x` falls below the
#' support, `CTE(x)` approaches the mean; at or above the support maximum it is
#' undefined (`NaN`).
#' @examples
#' d <- dst_norm(0, 1)
#' eval_cte(d, at = c(-1, 0, 1, 2))
#' enframe_cte(d, at = c(-1, 0, 1, 2))
#' @family distributional representations
#' @rdname cte
#' @export
eval_cte <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "cte", at)
}

#' @rdname cte
#' @export
enframe_cte <- function(..., at, arg_name = ".arg", fn_prefix = "cte",
                        sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_cte
  )
}
