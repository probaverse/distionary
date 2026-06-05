#' Locate the lower reference of a conditional tail expectation
#'
#' Finds a point to the left of the support where the conditional tail
#' expectation has levelled off at the mean. Conditioning on `X > x` for any
#' `x` below the support is just `E[X]`, so the conditional tail expectation is
#' constant there; this constant is the mean.
#' @param cte The conditional tail expectation as a function of a threshold.
#' @param tol Tolerance for detecting that the function has levelled off.
#' @param maxsteps Maximum number of leftward doubling steps.
#' @returns A length-2 numeric: the mean `m` and the reference point `x_ref`.
#' @noRd
cte_left_reference <- function(cte, tol = 1e-7, maxsteps = 200L) {
  x <- 0
  c_prev <- cte(x)
  step <- 1
  for (i in seq_len(maxsteps)) {
    x_new <- x - step
    c_new <- cte(x_new)
    if (is.finite(c_new) && abs(c_new - c_prev) < tol) {
      return(c(m = c_new, x_ref = x_new))
    }
    c_prev <- c_new
    x <- x_new
    step <- step * 2
  }
  c(m = c_prev, x_ref = x)
}

#' Reconstruct the survival function from a conditional tail expectation
#'
#' Recovers `S(x)` from an intrinsic conditional tail expectation by
#' mean-residual-life inversion. Writing `e(x) = CTE(x) - x` for the mean
#' residual life and `phi(x) = E[(X - x)^+]`, the identity `phi'(x) = -S(x)`
#' together with `S(x) = phi(x) / e(x)` gives `d log phi / dx = -1 / e(x)`,
#' so
#'
#'   phi(x) = phi(x_ref) * exp(-\int_{x_ref}^x du / e(u)),   S(x) = phi(x) / e(x),
#'
#' where `x_ref` is a point below the support, at which `phi(x_ref) = m - x_ref`
#' with `m` the mean. The reconstruction is exact once `x_ref` reaches the
#' support; for distributions whose support is unbounded below it is numerical,
#' with accuracy set by how far `x_ref` extends into the lower tail.
#' @param cte The conditional tail expectation as a function of a threshold.
#' @param at Vector of values at which to evaluate the survival function.
#' @param tol Tolerance for numerical integration; small positive number.
#' @returns Numeric vector the same length as `at`, the survival function at
#' `at`.
#' @references Jones, M. C. (1994). Expectiles and M-quantiles are quantiles.
#' \emph{Statistics & Probability Letters}, 20(2), 149-153.
#' @noRd
reconstruct_survival_from_cte <- function(cte, at, tol = 1e-9) {
  ref <- cte_left_reference(cte)
  m <- ref[["m"]]
  x_ref <- ref[["x_ref"]]
  e <- function(u) cte(u) - u
  phi_ref <- m - x_ref
  s <- vapply(
    at,
    function(x) {
      if (x <= x_ref) {
        return(1)
      }
      integ <- distionary_integrate(
        function(u) 1 / e(u),
        lower = x_ref, upper = x, tol = tol
      )
      (phi_ref * exp(-integ)) / e(x)
    },
    numeric(1)
  )
  pmin(pmax(s, 0), 1)
}
