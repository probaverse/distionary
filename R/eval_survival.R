#' Survival Function
#'
#' Access a distribution's survival function.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param inequality Which inequality the probability is taken over:
#' `"strict"`, the usual survival function, is the probability of `X > at`;
#' `"weak"` is the probability of `X >= at`.
#'
#' Note that the canonical survival function is the strict one, where the
#' canonical cdf is the weak one: between them they carve the line in two, so
#' [eval_cdf()] and `eval_survival()` sum to 1 as they are ordinarily written.
#' As with the cdf, the two differ only where `at` is an atom.
#' @family distributional representations
#' @seealso [eval_prob_right()], which is this function said the other way
#' around, for when the direction matters more than the name.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_survival(d, at = 0:4)
#' enframe_survival(d, at = 0:4)
#'
#' # The Poisson has atoms, so the two inequalities differ there.
#' eval_survival(dst_pois(1.1), at = 0:4, inequality = "weak")
#' @rdname survival
#' @export
eval_survival <- function(
  distribution,
  at,
  ...,
  inequality = c("strict", "weak")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "survival")
  eval_property(
    distribution,
    "survival",
    at,
    variant = variant(inequality = inequality, .entry = "survival")
  )
}

#' @noRd
eval_survival_from_network <- function(
  distribution,
  at,
  inequality = c("strict", "weak")
) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "survival")
  # Whichever inequality is asked of the survival function, the cdf answers
  # the opposite one: the two split the line between them.
  opposite <- if (inequality == "strict") "weak" else "strict"
  p <- eval_cdf(distribution, at = at, inequality = opposite)
  if (is.null(p)) {
    return(NULL)
  }
  1 - p
}

#' @rdname survival
#' @export
enframe_survival <- function(
  ...,
  at,
  inequality = "strict",
  arg_name = ".arg",
  fn_prefix = "survival",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_survival,
    fn_args = list(inequality = inequality)
  )
}
