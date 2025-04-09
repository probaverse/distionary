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
#' @srrstats {G1.1} The only relevant algorithm is the quantile algorithm.
#' It's included in the documentation that this algorithm is not new, and
#' was found on Stack Overflow somewhere, but unfortunately cannot find
#' the location anymore. --> Copied to `eval_quantile()`.
#' @srrstats {PD3.3} *Return objects which include values generated
#' from optimisation algorithms should include information on
#' optimisation algorithm and performance, minimally including the name
#' of the algorithm used, the convergence tolerance, and the number of
#' iterations.* This is specified for the only relevant algorithm, the
#' quantile algorithm. --> Copied to `eval_from_network-quantile.R`.
#' @rdname quantile
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user
#' wants this behaviour, it should be explicitly specified in their code,
#' because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#' @srrstats {G2.16} This version of distionary does force the propagation of
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user
#' specification for length-stability, also because `Inf` and `-Inf` are
#' expected in some cases (e.g., the support of any Normal distribution).
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
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
