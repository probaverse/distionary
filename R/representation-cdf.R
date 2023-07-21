#' Evaluate a representation of a distribution
#'
#' These eval functions evaluate different representations of
#' univariate (1-variable) distributions.
#'
#' Representations include:
#'
#' - `eval_cdf()` evaluates the cumulative distribution function.
#' - `eval_survival()` evaluates the survival function (one minus the cdf).
#' - `eval_density()` evaluates the probability density function.
#' - `eval_pmf()` evaluates the probability mass function.
#' - `eval_hazard()` evaluates the hazard function, for continous distributions.
#' - `eval_chf()` evaluates the cumulative hazard function.
#' - `eval_quantile()` evaluates the quantile function.
#' - `eval_odds()` evaluates the odds function (`pmf / (1 - pmf)`).
#' - `eval_return()` evaluates quantiles having a specified return period
#'   (evaluating the return function at `rp` is the same as evaluating the
#'   quantile function at `1 - 1 / rp`).
#'
#' @param distribution A distribution, inheriting the class `"dst"`.
#' @param at A vector of values to evaluate the univariate distributional
#' representation at.
#' @return The evaluated representation as a vector of length matching the
#' length of `at`.
#' @order 1
#' @examples
#' eval_cdf(dst_norm(0, 1), at = -1:2)
#' eval_hazard(dst_exp(3.5), at = 0:4)
#' eval_chf(dst_weibull(1.3, 0.4), at = 0:4)
#' @rdname eval_uni
#' @export
eval_cdf <- function(distribution, at) UseMethod("eval_cdf")

#' Evaluate a representation of a multivariate distribution
#'
#' These eval functions evaluate different representations of
#' multivariate (>1-variable) distributions.
#'
#' Representations include:
#'
#' - `eval_multi_cdf()` evaluates the cumulative distribution function.
#' - `eval_multi_survival()` evaluates the survival function (one minus the cdf).
#' - `eval_multi_density()` evaluates the probability density function.
#' - `eval_multi_pmf()` evaluates the probability mass function.
#' - `eval_multi_odds()` evaluates the odds function (`pmf / (1 - pmf)`).
#'
#' When a distribution is bivariate (2-variable), convenience functions
#' have been defined for each `eval_multi_*` function, accessible by replacing
#' `multi` with `bi`: for example, `eval_bi_cdf()` is a convenience function
#' instead of `eval_multi_cdf()`.
#'
#' Note that not all representations on univariate distributions have a
#' multivariate counterpart: `eval_quantile()`, for example, is only defined for
#' univariate distributions.
#'
#' @param distribution A distribution, inheriting the class `"bidst"` or
#' `"multidst"`.
#' @param .l A list of vectors, one for each dimension of the distribution,
#' to evaluate the distributional representation at. Optionally, a single
#' vector of the same length, for evaluating the representation at a single
#' point.
#' @param x,y Vectors of values to evaluate the bivariate distributional
#' representation at.
#' @return The evaluated representation as a vector of length matching the
#' length of `at`.
#' @order 1
#' @seealso [eval_cdf(), eval_density(), etc.]
#' @examples
#' eval_cdf(dst_norm(0, 1), at = -1:2)
#' eval_hazard(dst_exp(3.5), at = 0:4)
#' eval_chf(dst_weibull(1.3, 0.4), at = 0:4)
#' @rdname eval_multi
#' @export
eval_multi_cdf <- function(distribution, .l) UseMethod("eval_multi_cdf")

#' @rdname eval_multi
#' @export
eval_bi_cdf <- function(distribution, x, y) UseMethod("eval_bi_cdf")
