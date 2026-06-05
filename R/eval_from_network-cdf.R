#' Evaluate a CDF from the Network
#'
#' Reconstructs a distribution's cdf when it is not intrinsically defined.
#' This handles the reverse directions of two representations: from an
#' intrinsically defined expectile function, or from an intrinsically defined
#' conditional tail expectation, to the cdf, which in turn unlocks the rest of
#' the network. An intrinsic expectile function takes precedence.
#'
#' @details For a distribution defined through its expectile function
#' `xi(tau)`, the mean is `m = xi(1/2)` and, writing `tau(x)` for the level
#' whose expectile is `x`, the partial moment is
#'
#'   phi(x) = (1 - tau(x)) * (x - m) / (2 * tau(x) - 1).
#'
#' The survival function is `S(x) = -phi'(x)`, computed by a central
#' difference, and the cdf is `1 - S(x)`. The reconstruction is numerical;
#' precision is weakest in a small neighbourhood of the mean, where
#' `tau(x)` is near 1/2.
#'
#' For a distribution defined through its conditional tail expectation, the cdf
#' is `1 - S(x)`, with `S` recovered by mean-residual-life inversion (see
#' `reconstruct_survival_from_cte()`).
#'
#' If the distribution has no intrinsic cdf, expectile function, or conditional
#' tail expectation, `NULL` is returned, matching the behaviour of a
#' representation that cannot be found.
#' @param distribution A distribution object.
#' @param at Vector of values at which to evaluate the cdf.
#' @param tol,maxiter Tolerance (a small positive number) and maximum number of
#' iterations (at least 1); length 1 vectors.
#' @returns The cdf of the distribution evaluated at `at`, or `NULL` if it
#' cannot be reconstructed.
#' @references Jones, M. C. (1994). Expectiles and M-quantiles are quantiles.
#' \emph{Statistics & Probability Letters}, 20(2), 149-153.
#' @noRd
eval_cdf_from_network <- function(distribution, at, tol = 1e-12,
                                  maxiter = 200) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  has_expectile <- is_intrinsic(distribution, "expectile")
  has_cte <- is_intrinsic(distribution, "cte")
  if (!has_expectile && !has_cte) {
    return(NULL)
  }
  checkmate::assert_numeric(tol, 0, len = 1)
  checkmate::assert_integerish(maxiter, lower = 1, len = 1)
  if (length(at) == 0) {
    return(numeric(0L))
  }
  if (has_expectile) {
    expectile <- representation_as_function(distribution, "expectile")
    m <- expectile(0.5)
    return(cpp_expectile_reverse_cdf(at, expectile, m, tol, as.integer(maxiter)))
  }
  cte <- representation_as_function(distribution, "cte")
  1 - reconstruct_survival_from_cte(cte, at, tol = tol)
}
