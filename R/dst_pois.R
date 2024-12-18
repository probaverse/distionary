#' Poisson Distribution
#'
#' Makes a distribution belonging to the family of
#' Poisson distributions.
#' @param lambda Mean of the Poisson distribution.
#' @returns A distribution.
#' dst_pois(1)
#' @export
dst_pois <- function(lambda) {
	if (lambda < 0) {
		stop("'lambda' parameter must be greater than 0")
	} else if (lambda == 0) {
		return(dst_degenerate(lambda))
	}
	dst_parametric(
	  "pois", lambda = lambda, .variable = "discrete", .env = "package:stats"
	)
}
