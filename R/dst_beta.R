#' Beta Distribution
#'
#' Makes a distribution belonging to the family of
#' beta distributions.
#' @param shape1,shape2 Parameters, greater than 0. Also known as alpha
#' and beta.
#' @returns A distribution.
#' @examples
#' dst_beta(2, 3)
#' @export
dst_beta <- function(shape1, shape2) {
	if (shape1 <= 0) {
		stop("shape1 must be positive.")
	}
	if (shape2 <= 0) {
		stop("shape2 must be positive.")
	}
	dst_parametric(
	  "beta", shape1 = shape1, shape2 = shape2,
	  .variable = "continuous", .env = "package:stats"
	)
}
