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

test_that("Odds matches some known values", {
  expect_equal(eval_odds(dst_bern(0.5), at = 1), 1)
  expect_equal(eval_odds(dst_bern(0.2), at = 1), 0.25)
  expect_equal(eval_odds(dst_bern(0.8), at = 1), 4)
  d <- dst_pois(4)
  expect_equal(
    eval_odds(d, at = 0:10),
    eval_pmf(d, at = 0:10) / (1 - eval_pmf(d, at = 0:10))
  )
})
