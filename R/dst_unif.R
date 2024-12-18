#' Uniform Distribution
#'
#' Makes a distribution belonging to the family of
#' Uniform distributions.
#'
#' @param min,max Minimum and maximum of the distribution.
#' @returns A distribution.
#' @examples
#' dst_unif(0, 1)
#' @export
dst_unif <- function(min, max) {
	if (max < min) {
		stop("Parameter 'min' must be less than 'max'.")
	}
	if (max == min) {
		return(dst_degenerate(min))
	}
  dst_parametric(
    "unif", min = min, max = max,
    .variable = "continuous", .env = "package:stats"
  )
}
