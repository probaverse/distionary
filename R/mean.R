#' Moments of a Distribution
#'
#' Get common moment-related quantities of a
#' distribution.
#'
#' @param x,distribution Distribution to compute moment of.
#' @param ... Other arguments to pass to `integrate()` when
#' deploying numerical algorithm.
#'
#' @details If there is no moment specified in the distribution
#' definition, then moments are calculated numerically using
#' \code{stats::integrate()}.
#'
#' @return A single numeric.
#' @examples
#' a <- dst_gpd(0, 1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' mean(a)
#' variance(b)
#' kurtosis_raw(c)
#' kurtosis_exc(c)
#' @rdname moments
#' @export
mean.dst <- function(x, ...) {
  qf <- representation_as_function(x, "quantile")
  int <- try(stats::integrate(qf, lower = 0, upper = 1, ...))
  if (inherits(int, "try-error")) {
    warning("Integral did not converge. This might mean that the mean does
            not exist, or that the integral simply did not converge.
            Returning NaN.")
    return(NaN)
  }
  int$value
}
