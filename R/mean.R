#' Moments of a Distribution
#'
#' Get common moment-related quantities of a
#' distribution: `mean`, `variance`, standard deviation (`stdev`),
#' `skewness`, and `kurtosis` or excess kurtosis (`kurtosis_exc`).
#' If these quantities are not supplied in the
#' distribution's definition, a numerical algorithm may be used.
#'
#' @param x,distribution Distribution to evaluate.
#' @param ... When calculating the mean via integration of the quantile
#' function, arguments passed to `stats::integrate()`.
#' @note Beware that if a quantity is being calculated numerically
#' for a non-continuous (e.g., discrete) distribution, the calculation
#' could be highly approximate. An upcoming version of distionary will
#' resolve this issue.
#' @details If there is no method associated with a subclass of
#' \code{x}, then moments are calculated using
#' \code{stats::integrate()} from the quantile function.
#'
#' @returns A single numeric.
#' @examples
#' a <- dst_gpd(1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' mean(a)
#' variance(b)
#' kurtosis(c)
#' kurtosis_exc(c)
#' @srrstats {PD3.4} *Use of routines to integrate probability
#' distributions should explicitly document conditions under which
#' integrals are expected to remain stable, and ideally include
#' pre-processing checks for potentially unstable behaviour.*
#' - Noted that the integration is generally intended for continuous
#'   distributions and does not work so well with discrete components yet.
#' @rdname moments
#' @export
mean.dst <- function(x, ...) {
  ellipsis::check_dots_empty()
  eval_property(x, "mean")
}
