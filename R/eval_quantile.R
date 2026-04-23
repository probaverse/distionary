#' Distribution Quantiles
#'
#' Access a distribution's quantiles.
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @examples
#' d <- dst_unif(0, 4)
#' eval_quantile(d, at = 1:9 / 10)
#' enframe_quantile(d, at = 1:9 / 10)
#' @family distributional representations
#' @details When a quantile function does not exist, an algorithm is
#' deployed that calculates the left inverse of the CDF. This algorithm
#' works by progressively cutting the specified range in half, moving
#' into the left or right half depending on where the solution is.
#' The algorithm is not currently fast and is subject to improvement,
#' and is a simple idea that has been passed around on the internet
#' here and there. Tolerance is less than 1e-9, unless the maximum
#' number of iterations (200) is reached.
#'
#' The algorithm is not new, and is rather simple.
#' The algorithm works by progressively
#' cutting an initially wide range in half, moving into the left or right
#' half depending on where the solution is. I found the idea on Stack
#' Overflow somewhere, but unfortunately cannot find the location anymore.
#' @rdname quantile
#' @export
eval_quantile <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  eval_property(distribution, "quantile", at)
}

#' @rdname quantile
#' @export
enframe_quantile <- function(
    ..., at, arg_name = ".arg", fn_prefix = "quantile",
    sep = "_") {
  enframe_general(
    ...,
    at = at, arg_name = arg_name, fn_prefix = fn_prefix,
    sep = sep, eval_fn = eval_quantile
  )
}
