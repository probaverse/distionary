#' Poisson Distribution
#'
#' Makes a distribution belonging to the family of
#' Poisson distributions.
#' @param lambda Mean of the Poisson distribution.
#' @return Object of class "dst".
#' dst_pois(1)
#' @export
dst_pois <- function(lambda) {
	if (lambda < 0) {
		stop("'lambda' parameter must be greater than 0")
	} else if (lambda == 0) {
		return(dst_degenerate(lambda))
	}
  distribution(
    parameters = list(lambda = lambda),
    pmf = \(x) stats::dpois(x, lambda = lambda),
    cdf = \(x) stats::ppois(x, lambda = lambda),
    quantile = \(p) stats::qpois(p, lambda = lambda),
    realise = \(n) stats::rpois(n, lambda = lambda),
    survival = \(x) stats::ppois(x, lambda = lambda, lower.tail = FALSE),
    mean = lambda,
    variance = lambda,
    skewness = lambda^(-0.5),
    kurtosis_exc = 1 / lambda,
    range = c(0, Inf),
    .name = "Poisson",
    .vtype = "discrete"
  )
}
