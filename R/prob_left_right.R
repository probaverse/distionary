#' Find the probability left or right of a number
#'
#' Probability to the left or right of a number, inclusive or not.
#' `prob_left()` is a more general cdf defined using either `<` or `<=`, and
#' `prob_right()` is a more general survival function defined using either
#' `>` or `>=`.
#'
#' @param distribution Distribution to find probabilities of.
#' @param of Find the probability to the left or right *of* this number.
#' Could be a vector.
#' @param inclusive Should `of` be included in the probability calculation?
#' Logical.
#' @returns A vector of probabilities.
#' @rdname flexible_cdf
#' @examples
#' d <- dst_pois(5)
#' prob_left(d, of = 3, inclusive = TRUE)
#' prob_left(d, of = 3, inclusive = FALSE)
#' prob_right(d, of = 0:3, inclusive = TRUE)
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user
#' wants this behaviour, it should be explicitly specified in their code,
#' because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#' @srrstats {G2.16} This version of distionary does force the propagation of
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user
#' specification for length-stability, also because `Inf` and `-Inf` are
#' expected in some cases (e.g., the support of any Normal distribution).
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
#' @export
prob_left <- function(distribution, of, inclusive) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(of)
  checkmate::assert_logical(inclusive, len = 1)
  p_left <- eval_cdf(distribution, at = of)
  if (!inclusive) {
    p_break <- eval_pmf(distribution, at = of)
    p_left <- p_left - p_break
  }
  p_left
}

#' @rdname flexible_cdf
#' @export
prob_right <- function(distribution, of, inclusive) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(of)
  checkmate::assert_logical(inclusive, len = 1)
  p_right <- eval_survival(distribution, at = of)
  if (inclusive) {
    p_break <- eval_pmf(distribution, at = of)
    p_right <- p_right + p_break
  }
  p_right
}
