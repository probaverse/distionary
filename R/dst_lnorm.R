#' Log Normal Distribution
#'
#' Makes a distribution belonging to the family of
#' Log Normal distributions.
#' @param meanlog,variancelog Mean and variance of the distribution
#' on a log scale.
#' @returns A distribution.
#' dst_lnorm(0, 1)
#' @export
dst_lnorm <- function(meanlog, variancelog) {
	if (!inherits(variancelog, "try-error")) {
		if (variancelog == 0) {
			return(dst_degenerate(exp(meanlog)))
		}
		if (variancelog < 0) {
			stop("'variancelog' parameter must be non-negative.")
		}
	}
	dst_parametric(
	  "lnorm", meanlog = meanlog, sdlog = sqrt(variancelog),
	  .variable = "continuous", .env = "package:stats"
	)
}
