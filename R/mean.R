#' Moments of a Distribution
#'
#' Get common moment-related quantities of a
#' distribution: mean, variance, standard deviation (stdev),
#' skewness, and kurtosis. If these quantities are not supplied in the
#' distribution's definition, they will be calculated numerically.
#'
#' @param x Distribution to evaluate.
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
#' @return A single numeric.
#' @examples
#' a <- dst_gpd(1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' mean(a)
#' variance(b)
#' kurtosis(c)
#' kurtosis_exc(c)
#' @rdname moments
#' @export
mean.dst <- function(x, ...) {
  ellipsis::check_dots_empty()
  eval_representation(x, "mean")
}

eval_mean_from_network <- function(distribution, ...) {
  if (attr(distribution, "name") %in% c(
    "Hypergeometric", "Bernoulli", "Binomial"
  )) {
    # This case is for double-checking the moments supplied for these
    # distributions, and will be included until discretes handling is
    # implemented.
    r <- range(distribution)
    x <- seq(r[1], r[2], by = 1L)
    p <- eval_pmf(distribution, at = x)
    return(sum(p * x))
  }
  qf <- distribution[["quantile"]]
  if (is.null(qf)) {
    qf <- \(x) eval_quantile_from_network(distribution, x)
  }
  int <- try(
    stats::integrate(
      qf, lower = 0, upper = 1, rel.tol = 1e-09, subdivisions = 200L, ...
    ),
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    return(NaN)
  }
  int$value
}
