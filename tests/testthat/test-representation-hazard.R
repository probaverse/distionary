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


#' Test that the hazard function is correct by comparing it to the limit of
#' the probability of an event happening immediately, given that it hasn't
#' happened already, using the density function.
test_that("Hazard function is validated using the density function.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) == "continuous") {
        p <- 1:9 / 10
        x <- eval_quantile(d, at = p)
        r <- range(d)
        supposed_hazard <- eval_hazard(d, at = x)
        expect_equal(
          eval_density(d, at = x) / eval_survival(d, at = x),
          supposed_hazard
        )
      }
    }
  }
})
