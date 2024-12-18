#' Normal (Gaussian) Distribution
#'
#' Makes a distribution belonging to the family of
#' Normal (Gaussian) distributions.
#'
#' @param mean,variance Mean and variance of the distribution.
#' @returns A distribution.
#' @examples
#' dst_norm(0, 1)
#' @export
dst_norm <- function(mean, variance) {
  if (variance == 0) {
    return(dst_degenerate(mean))
  }
  if (variance < 0) {
    stop("'variance' parameter must be non-negative.")
  }
	dst_parametric(
	  "norm", mean = mean, sd = sqrt(variance),
	  .variable = "continuous", .env = "package:stats"
	)
}
