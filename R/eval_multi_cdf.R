#' @param .l A list of vectors, one for each dimension of the distribution,
#' to evaluate the distributional representation at. Optionally, a single
#' vector of the same length, for evaluating the representation at a single
#' point.
#' @rdname cdf
#' @export
eval_multi_cdf <- function(distribution, .l) UseMethod("eval_multi_cdf")

#' Evaluate a univariate distribution in a multivariate framework
#'
#' This method allows a user to use a multivariate evaluation function for
#' a univariate distribution.
#'
#' @inheritParams cdf
eval_multi_cdf.bidst <- function(distribution, .l) {
  if (length(.l) == 2) {
    stop("For a bivariate distribution, `.l` cannot have ", length(.l),
         " entries.")
  }
  eval_bi_cdf(distribution, x = .l[[1]], y = .l[[2]])
}

#' Evaluate a univariate distribution in a multivariate framework
#'
#' This method allows a user to use a multivariate evaluation function for
#' a univariate distribution.
#'
#' @inheritParams cdf
eval_multi_cdf.dst <- function(distribution, .l) {
  if (length(.l) > 1) {
    stop("For a univariate distribution, `.l` cannot have ", length(.l),
         " entries.")
  }
  at <- .l[[1]]
  eval_cdf(distribution, at = at)
}

#' @export
eval_cdf.default <- function(distribution, .l) {
  stop("Can't find a cdf for this distribution.")
}
