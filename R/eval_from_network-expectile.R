#' Evaluate Expectiles from the Network
#'
#' Computes a distribution's expectiles from its survival function and mean,
#' for distributions that do not define an expectile function intrinsically.
#' This is the forward direction: from the existing network of properties to
#' the expectile function.
#'
#' @details The `at`-expectile is the unique root in `x` of
#'
#'   g(x) = (2 * at - 1) / (1 - at) * phi(x) + mean - x,
#'
#' where `phi(x) = E[(X - x)^+]` is obtained by integrating the survival
#' function from `x` upward. The function `g` is continuous and strictly
#' decreasing, so its root is found with a safeguarded Newton-Raphson
#' iteration whose derivative `-((2 * at - 1) / (1 - at)) * survival(x) - 1`
#' uses the identity `phi'(x) = -survival(x)`.
#'
#' The computation is numerical and works for continuous and mixed
#' distributions. Exact computation for discrete components, which requires
#' enumerating the support, is left for a future version.
#' @param distribution A distribution with an accessible survival function and
#' a finite mean.
#' @param at Vector of expectile levels in (0, 1).
#' @param tol,maxiter Tolerance (a small positive number) and maximum number of
#' iterations (at least 1); length 1 vectors.
#' @returns The `at`-expectiles of the distribution; numeric vector the same
#' length as `at`.
#' @references Daouia, A., Stupfler, G., & Usseglio-Carleve, A. (2023). An
#' expectile computation cookbook. \emph{TSE Working Paper No. 23-1458}.
#' @noRd
eval_expectile_from_network <- function(distribution, at, tol = 1e-9,
                                        maxiter = 200) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  checkmate::assert_numeric(tol, 0, len = 1)
  checkmate::assert_integerish(maxiter, lower = 1, len = 1)
  if (length(at) == 0) {
    return(numeric(0L))
  }
  if (vtype(distribution) != "continuous") {
    stop(
      "Expectiles of discrete or mixed distributions are well-defined, but ",
      "this version of distionary can only compute them numerically for ",
      "continuous distributions (the survival function is integrated, which ",
      "fails on the steps of a discrete distribution). Exact computation for ",
      "distributions with discrete components is planned."
    )
  }
  m <- eval_property(distribution, "mean")
  if (is.null(m) || !is.finite(m)) {
    stop(
      "Expectiles require a finite mean, which this distribution does ",
      "not have or cannot compute."
    )
  }
  survival <- representation_as_function(distribution, "survival")
  upper <- range(distribution)[2L]
  phi <- function(x) {
    partial_moment(distribution, x, tol = tol, survival = survival, upper = upper)
  }
  cpp_expectile_forward(at, phi, survival, m, tol, as.integer(maxiter))
}
