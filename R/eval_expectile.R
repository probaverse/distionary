#' Distribution Expectiles
#'
#' Access a distribution's expectiles.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @details The expectile function is the least-squares analogue of the
#' quantile function: the `at`-expectile is the value `x` that balances the
#' `at`-weighted mean distance to outcomes above `x` against the
#' `(1 - at)`-weighted mean distance to outcomes below. The
#' 1/2-expectile is the mean. Expectiles characterise a distribution with a
#' finite first moment in the same way quantiles do, so a distribution can be
#' specified through its expectile function alone, from which the cdf and the
#' rest of the network can be recovered.
#'
#' When an expectile function is not intrinsically defined, it is computed
#' from the survival function and the mean by solving the expectile
#' identification equation. The solver is a safeguarded Newton-Raphson
#' iteration; the survival function is integrated numerically to obtain the
#' required partial moment. For distributions with discrete components this
#' computation is numerical rather than exact.
#' @references Daouia, A., Stupfler, G., & Usseglio-Carleve, A. (2023). An
#' expectile computation cookbook. \emph{TSE Working Paper No. 23-1458}.
#' @examples
#' d <- dst_norm(0, 1)
#' eval_expectile(d, at = 1:9 / 10)
#' enframe_expectile(d, at = 1:9 / 10)
#' @family distributional representations
#' @rdname expectile
#' @export
eval_expectile <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "expectile", at)
}

#' @rdname expectile
#' @export
enframe_expectile <- function(
  ..., at, arg_name = ".arg", fn_prefix = "expectile",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_expectile
  )
}
