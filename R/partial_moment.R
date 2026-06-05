#' Upper partial moment (stop-loss transform)
#'
#' Evaluates `phi(x) = E[(X - x)^+] = \int_x^\infty S(t) dt`, where `S` is the
#' survival function. This is the quantity that links the survival function to
#' both the expectile function and the conditional tail expectation, so it is
#' factored out here for reuse.
#' @param distribution A distribution with an accessible survival function.
#' @param at Vector of values at which to evaluate the partial moment.
#' @param tol Tolerance for numerical integration; small positive number.
#' @param survival Optional survival function, supplied to avoid recomputing it
#' on repeated calls. Defaults to the distribution's survival function.
#' @param upper Optional upper limit of the support, supplied to avoid
#' recomputing it. Defaults to the distribution's range maximum.
#' @returns Numeric vector the same length as `at`.
#' @noRd
partial_moment <- function(distribution, at, tol = 1e-9, survival = NULL,
                           upper = NULL) {
  if (is.null(survival)) {
    survival <- representation_as_function(distribution, "survival")
  }
  if (is.null(upper)) {
    upper <- range(distribution)[2L]
  }
  vapply(
    at,
    function(x) {
      if (x >= upper) {
        return(0)
      }
      distionary_integrate(survival, lower = x, upper = upper, tol = tol)
    },
    numeric(1)
  )
}
