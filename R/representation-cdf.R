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
eval_cdf <- function(distribution, at) {
  # Check `at` and `distribution`
  eval_cdf_clean(distribution, at)
}

#' Evaluate a representation using checked inputs
#'
#' This is an internal generic that dispatches the appropriate representation
#' and anticipates cleaned inputs made by the user-facing functions without
#' the `_clean` suffix.
#'
#' @inheritParams eval_cdf
#' @details
#' Inputs are expected to be processed:
#'
#' - Dimension of the distribution must match the length of `at`.
#' - If the distribution has dimension 1, `at` is an atomic vector;
#'   if dimension >1, `at` is a list of vectors.
#' - Vectors in `at` must be recycled to the same length.
eval_cdf_clean <- function(distribution, at) UseMethod("eval_cdf_clean")

#' @export
eval_cdf_clean.default <- function(distribution, at) {
  stop("Can't find a cdf for this distribution.")
}
