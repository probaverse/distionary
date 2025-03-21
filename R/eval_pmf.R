#' Probability Mass Function
#'
#' Access a distribution's probability mass function (pmf).
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_pois(5)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' enframe_pmf(d, at = 0:4)
#' eval_pmf(dst_norm(0, 1), at = -3:3)
#' @family distributional representations
#' @rdname pmf
#' @export
eval_pmf <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
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
enframe_pmf <- function(..., at, arg_name = ".arg", fn_prefix = "pmf",
                        sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_pmf
  )
}
