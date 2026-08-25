#' Cumulative Distribution Function
#'
#' Access a distribution's cumulative distribution function (cdf).
#'
#' @param distribution,... A distribution, or possibly multiple
#' distributions in the case of `...`.
#' @param at Vector of values to evaluate the representation at.
#' @param inequality Which inequality the probability is taken over:
#' `"weak"`, the usual cdf, is the probability of `X <= at`; `"strict"` is the
#' probability of `X < at`.
#'
#' The two differ only where `at` is an atom, since only there does the point
#' itself carry probability. For a distribution with no atoms they agree
#' everywhere, and the argument makes no difference.
#' @param arg_name For `enframe_`, name of the column containing
#' the function arguments. Length 1 character vector.
#' @param fn_prefix For `enframe_`, name of the function to
#' appear in the column(s). Length 1 character vector.
#' @param sep When `enframe`'ing more than one distribution, the
#' character that will be separating the `fn_name` and the distribution name.
#' Length 1 character vector.
#' @returns The evaluated representation in vector form (for `eval_`)
#' with length matching the length of `at`, and data frame
#' or tibble form (for `enframe_`) with number of rows matching the
#' length of `at`. The `at` input occupies the first column,
#' named `.arg` by default, or the specification in `arg_name`;
#' the evaluated representations for each distribution in `...`
#' go in the subsequent columns (one column per distribution). For a
#' single distribution, this column is named according to the
#' representation by default (cdf, survival, quantile, etc.),
#' or the value in `fn_prefix`. For multiple distributions, unnamed
#' distributions are auto-named, and columns are named
#' `<fn_prefix><sep><distribution_name>` (e.g., `cdf_distribution1`).
#' @family distributional representations
#' @seealso [eval_prob_left()], which is this function said the other way
#' around, for when the direction matters more than the name.
#' @examples
#' d1 <- dst_unif(0, 4)
#' d2 <- dst_pois(1.1)
#' eval_cdf(d1, at = 0:4)
#' enframe_cdf(d1, at = 0:4)
#' enframe_cdf(d1, d2, at = 0:4)
#' enframe_cdf(model1 = d1, model2 = d2, at = 0:4)
#' enframe_cdf(
#'   model1 = d1, model2 = d2, at = 0:4, arg_name = "value"
#' )
#'
#' # The Poisson has atoms, so the two inequalities differ there.
#' eval_cdf(d2, at = 0:4, inequality = "strict")
#' @rdname cdf
#' @export
eval_cdf <- function(
  distribution,
  at,
  ...,
  inequality = c("weak", "strict")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "cdf")
  eval_property(
    distribution,
    "cdf",
    at,
    variant = variant(inequality = inequality, .entry = "cdf")
  )
}

#' @noRd
eval_cdf_from_network <- function(
  distribution,
  at,
  inequality = c("weak", "strict")
) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  inequality <- match_variant(inequality, "inequality", "cdf")
  if (inequality == "weak") {
    # There is nothing to derive a cdf from: it is the one representation
    # `distribution()` insists on, and a distribution lacking it has always
    # given `NULL` here.
    return(NULL)
  }
  # Strictly below a point is at-or-below it, less whatever sits on it.
  p <- eval_cdf(distribution, at = at)
  if (is.null(p)) {
    return(NULL)
  }
  p - eval_pmf(distribution, at = at)
}

#' @rdname cdf
#' @export
enframe_cdf <- function(
  ...,
  at,
  inequality = "weak",
  arg_name = ".arg",
  fn_prefix = "cdf",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_cdf,
    fn_args = list(inequality = inequality)
  )
}
