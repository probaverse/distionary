#' Log Pearson Type III distribution
#'
#' Makes a Log Pearson Type III distribution, which is the
#' distribution of the exponential of a random variable following
#' a Pearson Type III distribution.
#'
#' @param meanlog Mean of the log of the random variable; single numeric.
#' @param sdlog Standard deviation of the log of the random variable;
#' single positive numeric.
#' @param skew Skewness of the log of the random variable;
#' single numeric.
#' @returns A Log Pearson Type III distribution.
#' @examples
#' dst_lp3(0, 1, 1)
#' @srrstats {PD3.0} *Manipulation of probability distributions should
#' very generally be analytic, with numeric manipulations only
#' implemented with clear justification (ideally including references).*
#' This only applies to the Pearson Type III and Log Pearson Type III
#' that manipulates the Gamma distribution from the stats package analytically.
#' User-facing manipulation is the job of the `distplyr` package in the
#' probaverse family. --> Copied to `dst_lp3.R`.
#' @srrstats {G2.0} Assertions on lengths of inputs (asserting that
#' inputs expected to be single- or multi-valued) are explicitly
#' tested for distribution parameters; implicitly through evaluation
#' functions.
#' @srrstats {G2.0a} Explicit secondary documentation of expectations on
#' lengths of inputs have been provided where relevant. See `dst_norm()`
#' for an example.
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.1a} Explicit secondary documentation of expectations on
#' data types of all vector inputs are provided. See `dst_norm()` for an
#' example.
#' @srrstats {G2.2} Prohibiting or restricting submission of multivariate
#' input (i.e., distributions) to univariate parameters is
#' done using the checkmate package for relevant functions (e.g., `dst_*()`
#' specifications)
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.6} distionary asserts one-dimensional input where required
#' (e.g., `dst_*()` specifications) using the checkmate package.
#' @srrstats {G2.13} Checks for missing data are conducted for distribution
#' parameters and a Null distribution is made to handle missing data.
#' See `dst_norm()` for an example. Checks are made for built-in
#' representations, but the onus is on the user for self-defined
#' distributions.
#' @srrstats {G2.15} Functions never assume non-missingness, and never
#' pass arguments to another function with `na.rm = FALSE`-type parameters.
#' This is most relevant for functions like `dst_norm()`.
#' @export
dst_lp3 <- function(meanlog, sdlog, skew) {
  checkmate::assert_numeric(meanlog, len = 1)
  checkmate::assert_numeric(sdlog, 0, len = 1)
  checkmate::assert_numeric(skew, len = 1)
  if (is.na(meanlog) || is.na(sdlog) || is.na(skew)) {
    return(dst_null())
  }
  # skewness = 2 / sqrt(shape), shape = 4 / skewness^2
  shape <- 4 / skew^2
  # sd = sqrt(shape) * scale
  scale <- sdlog / sqrt(shape)
  # mean = scale * shape
  shift <- meanlog - scale * shape
    distribution(
      .parameters = list(meanlog = meanlog, sdlog = sdlog, skew = skew),
      cdf = function(x) stats::pgamma(
        log(pmax(0, x)) - shift,
        shape = shape, scale = scale
      ),
      survival = function(x) stats::pgamma(
        log(pmax(0, x)) - shift,
        shape = shape, scale = scale, lower.tail = FALSE
      ),
      density = function(x) {
        res <- stats::dgamma(
          log(pmax(0, x)) - shift,
          shape = shape, scale = scale
        ) / x
        res[x == 0] <- 0
        res
      },
      quantile = function(p) exp(
        stats::qgamma(p, shape = shape, scale = scale) + shift
      ),
      realise = function(n) exp(
        stats::rgamma(n, shape = shape, scale = scale) + shift
      ),
      .name = "Log Pearson Type III",
      .vtype = "continuous"
    )
}
