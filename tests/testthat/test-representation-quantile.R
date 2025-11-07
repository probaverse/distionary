#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance.
#' @srrstats {G5.8d} Data outside of the scope of the (quantile) algorithm
#' is only applicable when the quantile probability is outside of [0, 1],
#' in which case an error is thrown (due to a check for valid function
#' inputs).
#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters.
#' @srrstats {G5.4b} Implementations of existing methods (cdf, density, ...)
#' are compared against the stats package where possible. Implementations
#' like the hazard function that are not found in the stats package are
#' implemented formulaically and verified by comparing to algorithm based
#' on the representation's definition.
#' @srrstats {G5.6} Parameter recovery is relevant when distributional
#' properties (like quantiles) are computed from other properties (like the
#' cdf); these are possible to validate by comparing the intrinsic version
#' of the property against the derived version as if it were absent. See,
#' for example, `test-representation-density.R` (copied there).
#' @srrstats {G5.6a} Parameter recovery tests are conducted using the default
#' tolerance in the `testthat::expect_equal()` function whenever tests of
#' user-facing outputs are evaluated.
#' @srrstats {PD4.3} Tests of optimisation or integration algorithms
#' compare derived results from built-in results for permutations of
#' every distribution parameter.
#' @srrstats {PD4.4} Tests of optimisation or integration algorithms
#' compare derived results with algorithms in the stats package (e.g.,
#' quantile algorithm compared to `stats::q*()` functions).

test_that("Quantile function calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      p <- 1:99 / 100
      if (is_intrinsic(d, "quantile")) {
        if (vtype(d) == "continuous") {
          expect_equal(
            eval_quantile_from_network(d, at = p),
            eval_quantile(d, at = p)
          )
        } else if (pretty_name(d) == "Finite") {
          expect_error(eval_quantile_from_network(d, at = 0.4))
          support <- parameters(d)$outcomes
          x <- support[1] - 1
          q_derived <- numeric(0L)
          j <- 0
          for (i in seq_along(p)) {
            p_ <- p[i]
            below <- eval_cdf(d, at = x) < p_
            while (below) {
              j <- j + 1
              x <- support[j]
              below <- eval_cdf(d, at = x) < p_
            }
            q_derived[i] <- x
          }
          expect_equal(
            q_derived,
            eval_quantile(d, at = p)
          )
        } else if (pretty_name(d) != "Degenerate") {
          expect_error(eval_quantile_from_network(d, at = 0.4))
          # Distributions in distionary start at 0 at least.
          x <- -1
          q_derived <- numeric(0L)
          for (i in seq_along(p)) {
            p_ <- p[i]
            below <- eval_cdf(d, at = x) < p_
            while (below) {
              x <- x + 1
              below <- eval_cdf(d, at = x) < p_
            }
            q_derived[i] <- x
          }
          expect_equal(
            q_derived,
            eval_quantile(d, at = p)
          )
        }
      }
    }
  }
})

#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance. --> Copied to `test-representation-quantile.R`.
test_that("Quantile algorithm takes longer for smaller tolerance.", {
  d <- dst_norm(0, 1)
  p <- 1:99 / 100
  t1 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-4))
  t2 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-8))
  expect_lt(t1[["elapsed"]], t2[["elapsed"]])
})
