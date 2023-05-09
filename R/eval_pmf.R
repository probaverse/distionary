#' Probability Mass Function
#'
#' Access a distribution's probability mass function (pmf).
#'
#' @inheritParams eval_cdf
#' @return The evaluated probabilities
#' in vector form (for `eval_`) and data frame
#' or tibble form (for `enframe_`).
#' @examples
#' d <- dst_empirical(1:10)
#' eval_pmf(d, at = c(1, 2, 2.5))
#' enframe_pmf(d, at = 0:4)
#' eval_pmf(dst_norm(0, 1), at = -3:3, strict = FALSE)
#' @family distributional representations
#' @rdname pmf
#' @export
eval_pmf <- function(distribution, ...) UseMethod("eval_pmf")

#' @export
eval_pmf.dst <- function(distribution, at, strict = TRUE) {
  if (variable(distribution) == "discrete") {
    stop("Cannot find the pmf for this distribution.")
  }
  if (strict) {
    stop("This distribution does not have a pmf. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (variable(distribution) == "continuous") {
      return(rep(0, length(at)))
    } else {
      stop("Cannot find probabilities for this distribution.")
    }
  }
}

#' @rdname pmf
#' @export
enframe_pmf <- function(..., at, arg_name = ".arg", fn_prefix = "pmf",
							sep = "_", strict = TRUE) {
	enframe_general(..., at = at, arg_name = arg_name, fn_prefix = fn_prefix,
					sep = sep, eval_fn = eval_pmf,
					fn_args = list(strict = strict))
}
