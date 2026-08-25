#' Probability Mass Function
#'
#' Access a distribution's probability mass function (pmf).
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param definition Which definition of a mass function to hold the
#' distribution to. Under `"extended"`, the default, the mass at a point is
#' the size of the cdf's jump there, which is an answer any distribution can
#' give --- zero, for a point carrying no probability. Under `"strict"`, it
#' is a mass function in the full sense --- masses that account for all of
#' the distribution's probability --- which only a discrete distribution has,
#' and asking a distribution with regions for one is an error.
#'
#' This is a question about the distribution, not about which function to
#' evaluate: where both definitions apply they give the same numbers, and
#' `"strict"` only refuses where a mass function does not exist.
#' @examples
#' d <- dst_pois(5)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' enframe_pmf(d, at = 0:4)
#'
#' # A continuous distribution puts no probability on any one point,
#' # so it has no mass function in the strict sense.
#' eval_pmf(dst_norm(0, 1), at = -3:3)
#' try(eval_pmf(dst_norm(0, 1), at = -3:3, definition = "strict"))
#' @family distributional representations
#' @rdname pmf
#' @export
eval_pmf <- function(
  distribution,
  at,
  ...,
  definition = c("extended", "strict")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  definition <- match_variant(definition, "definition", "pmf")
  if (definition == "strict") {
    check_strict_definition(distribution, "pmf")
  }
  eval_property(distribution, "pmf", at)
}

#' @noRd
eval_pmf_from_network <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  if (vtype(distribution) == "continuous") {
    at[!is.na(at)] <- 0
    return(at)
  }
  stop("Cannot find pmf, which must be specified in the distribution.")
}

#' @rdname pmf
#' @export
enframe_pmf <- function(
  ...,
  at,
  definition = "extended",
  arg_name = ".arg",
  fn_prefix = "pmf",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_pmf,
    fn_args = list(definition = definition)
  )
}
