#' Find the probability left or right of a number
#'
#' Probability to the left or right of a number, inclusive or not.
#' `prob_left()` is a more general cdf defined using either `<` or `<=`, and
#' `prob_right()` is a more general survival function defined using either
#' `>` or `>=`.
#'
#' @param distribution Distribution to find probabilities of.
#' @param of Find the probability to the left or right *of* this number.
#' Could be a vector.
#' @param inclusive Should `of` be included in the probability calculation?
#' Logical.
#' @rdname flexible_cdf
#' @export
prob_left <- function(distribution, of, inclusive) {
	p_left <- eval_cdf(distribution, at = of)
	if (!inclusive) {
		p_break <- eval_pmf(distribution, at = of, strict = FALSE)
		p_left <- p_left - p_break
	}
	p_left
}

#' @rdname flexible_cdf
#' @export
prob_right <- function(distribution, of, inclusive) {
	p_right <- eval_survival(distribution, at = of)
	if (inclusive) {
		p_break <- eval_pmf(distribution, at = of, strict = FALSE)
		p_right <- p_right + p_break
	}
	p_right
}
