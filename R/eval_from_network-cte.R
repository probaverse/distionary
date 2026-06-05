#' Evaluate a Conditional Tail Expectation from the Network
#'
#' Computes a distribution's conditional tail expectation from its survival
#' function and mean, for distributions that do not define one intrinsically.
#' This is the forward direction: from the existing network to the conditional
#' tail expectation.
#'
#' @details The threshold-`at` conditional tail expectation is
#'
#'   CTE(at) = at + phi(at) / S(at),
#'
#' where `phi(at) = E[(X - at)^+]` is the upper partial moment, obtained by
#' integrating the survival function from `at` upward, and `S` is the survival
#' function. Below the support the value equals the mean; at or above the
#' support maximum the survival probability is zero and the value is `NaN`.
#' @param distribution A distribution with an accessible survival function and
#' a finite mean.
#' @param at Vector of thresholds.
#' @param tol Tolerance for numerical integration; small positive number.
#' @returns The conditional tail expectation of the distribution at `at`;
#' numeric vector the same length as `at`.
#' @noRd
eval_cte_from_network <- function(distribution, at, tol = 1e-9) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  checkmate::assert_numeric(tol, 0, len = 1)
  if (length(at) == 0) {
    return(numeric(0L))
  }
  if (vtype(distribution) != "continuous") {
    stop(
      "The conditional tail expectation of discrete or mixed distributions ",
      "is well-defined, but this version of distionary can only compute it ",
      "numerically for continuous distributions (the survival function is ",
      "integrated, which fails on the steps of a discrete distribution). ",
      "Exact computation for distributions with discrete components is planned."
    )
  }
  m <- eval_property(distribution, "mean")
  if (is.null(m) || !is.finite(m)) {
    stop(
      "The conditional tail expectation requires a finite mean, which this ",
      "distribution does not have or cannot compute."
    )
  }
  survival <- representation_as_function(distribution, "survival")
  upper <- range(distribution)[2L]
  s <- survival(at)
  phi <- partial_moment(distribution, at,
    tol = tol, survival = survival,
    upper = upper
  )
  out <- at + phi / s
  out[s <= 0] <- NaN
  out
}
