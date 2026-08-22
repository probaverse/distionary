#' Probability to the Left or Right of a Number
#'
#' The probability that a distribution falls to one side of a value.
#' `eval_prob_left()` is the cdf and `eval_prob_right()` the survival
#' function, named for the direction rather than the convention, and free to
#' take either inequality.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param inequality Which inequality the probability is taken over. For
#' `eval_prob_left()`, `"weak"` is the probability of `X <= at` and
#' `"strict"` the probability of `X < at`; for `eval_prob_right()`,
#' `"strict"` is the probability of `X > at` and `"weak"` the probability of
#' `X >= at`.
#'
#' The defaults are the ones that make each function its familiar
#' counterpart --- [eval_cdf()] and [eval_survival()] respectively --- and
#' the choice only makes a difference where `at` is an atom.
#' @details
#' These are the same probabilities [eval_cdf()] and [eval_survival()] give,
#' under names that say which way they point. Which pair to reach for is a
#' matter of what the code around them is about: a survival analysis is
#' written in survival functions, while a question about whether a value
#' falls short of a threshold reads better as the probability to its left.
#' @examples
#' d <- dst_pois(5)
#' eval_prob_left(d, at = 3)
#' eval_prob_left(d, at = 3, inequality = "strict")
#' eval_prob_right(d, at = 0:3, inequality = "weak")
#' enframe_prob_left(d, at = 0:3)
#' @family distributional representations
#' @rdname prob_left_right
#' @export
eval_prob_left <- function(
  distribution,
  at,
  ...,
  inequality = c("weak", "strict")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "prob_left")
  eval_property(
    distribution,
    "prob_left",
    at,
    variant = variant(inequality = inequality, .entry = "prob_left")
  )
}

#' @noRd
eval_prob_left_from_network <- function(
  distribution,
  at,
  inequality = c("weak", "strict")
) {
  inequality <- match_variant(inequality, "inequality", "prob_left")
  eval_cdf(distribution, at = at, inequality = inequality)
}

#' @rdname prob_left_right
#' @export
eval_prob_right <- function(
  distribution,
  at,
  ...,
  inequality = c("strict", "weak")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "prob_right")
  eval_property(
    distribution,
    "prob_right",
    at,
    variant = variant(inequality = inequality, .entry = "prob_right")
  )
}

#' @noRd
eval_prob_right_from_network <- function(
  distribution,
  at,
  inequality = c("strict", "weak")
) {
  inequality <- match_variant(inequality, "inequality", "prob_right")
  eval_survival(distribution, at = at, inequality = inequality)
}

#' @rdname prob_left_right
#' @export
enframe_prob_left <- function(
  ...,
  at,
  inequality = "weak",
  arg_name = ".arg",
  fn_prefix = "prob_left",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_prob_left,
    fn_args = list(inequality = inequality)
  )
}

#' @rdname prob_left_right
#' @export
enframe_prob_right <- function(
  ...,
  at,
  inequality = "strict",
  arg_name = ".arg",
  fn_prefix = "prob_right",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_prob_right,
    fn_args = list(inequality = inequality)
  )
}

#' Find the probability left or right of a number
#'
#' `r lifecycle::badge("deprecated")` Renamed to [eval_prob_left()] and
#' [eval_prob_right()], which join the rest of the `eval_` family: they take
#' `at` rather than `of`, they have `enframe_` counterparts, and they say
#' which inequality they mean in words rather than as a logical.
#'
#' @param distribution Distribution to find probabilities of.
#' @param of Find the probability to the left or right *of* this number.
#' Could be a vector.
#' @param inclusive Should `of` be included in the probability calculation?
#' Logical.
#' @returns A vector of probabilities.
#' @rdname flexible_cdf
#' @keywords internal
#' @examples
#' d <- dst_pois(5)
#' # Deprecated:
#' # prob_left(d, of = 3, inclusive = TRUE)
#' # Instead:
#' eval_prob_left(d, at = 3, inequality = "weak")
#' @export
prob_left <- function(distribution, of, inclusive) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "prob_left()",
    with = "eval_prob_left()"
  )
  checkmate::assert_logical(inclusive, len = 1)
  eval_prob_left(
    distribution,
    at = of,
    inequality = if (inclusive) "weak" else "strict"
  )
}

#' @rdname flexible_cdf
#' @export
prob_right <- function(distribution, of, inclusive) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "prob_right()",
    with = "eval_prob_right()"
  )
  checkmate::assert_logical(inclusive, len = 1)
  eval_prob_right(
    distribution,
    at = of,
    inequality = if (inclusive) "weak" else "strict"
  )
}
