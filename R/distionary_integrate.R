#' Integration regime used in distionary
#'
#' Integrating distribution-related functions can be finicky. This function
#' is designed to get around the shortcomings of built-in integration
#' regimes like stats::integrate and cubature::hcubature.
#'
#' @param fun Function (integrand) to be integrated.
#' @param lower,upper Limits of integration. Can be infinite.
#' @param tol Tolerance; small positive number.
#' @param ... Other arguments to pass to `cubature::hcubature()`.
#' @returns A single numeric, the integral of the function.
#' @details Here are the integration shortfalls encountered with distributions:
#'
#' - The mean of a standard Cauchy distribution integrates to 0, when it
#'   should not exist. This happens because the function is symmetric and
#'   the negative part cancels out the positive part numerically, but
#'   both sides of the integral are not well-defined.
#'
#' The solution involves a simple `if` statement to break the integral in two.
#'
#' NOTE: if `cubature::hcubature()` is used, there have been other challenges,
#' and so it's best to use `stats::integrate()`.
#'
#' - Sometimes, if either end of the integral is finite but evaluates to
#'   infinity (like the integrand for calculating the mean of the
#'   Beta(0.5, 0.5) distribution through its density), the integral evaluates
#'   to infinity.
#' - The integrand for the Cauchy(0, 1) mean via the density function should
#'   be infinite on either side of x=0, but comes back as finite.
#'
#' **Change of variables math if having trouble integrating to a finite
#'   endpoint that evaluates to infinity**
#' Integral is I = int f(x) dx, x = x0..x1
#'
#' _Upper Integral_
#' Let t = 1 / (x1 - x)
#' ==> x = x1 - 1/t
#' t goes from 1 / (x1 - x0) .. Inf
#' dx = 1/t^2 dt
#' Integral becomes:
#' I = int f(x1 - 1/t) / t^2 dt
#'
#' _Lower Integral_
#' Let t = 1 / (x - x0)
#' ==> x = x0 + 1/t
#' dx = -1/t^2 dt
#' t goes from Inf .. 1 / (x1 - x0)
#' Integral becomes
#' I = int -f(x0 + 1/t) / t^2 dt
#' Or, switch the order of integration, and get
#' I = int  f(x0 + 1/t) / t^2 dt
#'
#' _Both_
#' When both finite endpoints have non-finite evaluation, split the
#' integral in half using the midpoint.
#'
#' @examples
#' # Beta(0.5, 0.5) should have mean of 0.5.
#' integrand <- \(x) x * stats::dbeta(x, 0.5, 0.5)
#' plot(integrand)
#' distionary_integrate(integrand, 0, 1)
#' stats::integrate(integrand, 0, 1)  # Good
#' cubature::hcubature(integrand, 0, 1, tol = 1e-7) # Bad
#' cubature::hcubature(integrand, 0, 1)  # Works but not as accurate
#'
#' # Cauchy(0, 1)
#' integrand <- \(x) x * stats::dcauchy(x)
#' plot(integrand, -10, 10)
#' distionary_integrate(integrand, 0, Inf) # NaN. Ideally would be Inf.
#' distionary_integrate(integrand, -Inf, Inf) # NaN, good.
#' stats::integrate(integrand, -Inf, Inf)  # 0, bad.
#' cubature::hcubature(integrand, -Inf, Inf) # Sometimes hangs, or NaN.
#' @noRd
distionary_integrate <- function(fun, lower, upper, tol = 1e-9, ...) {
  checkmate::assert_function(fun)
  checkmate::assert_numeric(lower, len = 1)
  checkmate::assert_numeric(upper, lower = lower, len = 1)
  checkmate::assert_numeric(tol, lower = 0, len = 1)
  if (lower == upper) return(0)
  if (is.infinite(lower) && is.infinite(upper)) {
    ## Solves the problem of the standard Cauchy mean evaluating to 0,
    ## and the t(3) distribution evaluating to finite skewness.
    i_lower <- distionary_integrate(
      fun,
      lower = lower, upper = 0,
      tol = tol / 2,
      ...
    )
    i_upper <- distionary_integrate(
      fun,
      lower = 0, upper = upper,
      tol = tol / 2,
      ...
    )
    return(i_lower + i_upper)
  }
  int <- try(
    stats::integrate(fun, lower, upper, rel.tol = tol, ...)$value,
    silent = TRUE
  )
  if (inherits(int, "try-error")) {
    return(NaN)
  }
  int
}

