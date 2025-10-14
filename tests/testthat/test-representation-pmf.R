# Validate PMF can be recovered from CDF
#
# Systematic testing uses the `test_distributions` object.
# To modify it, see `data-raw/test_distributions.R`.
# To update it, run `Rscript data-raw/test_distributions.R`.

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

test_that("PMF calculated thru CDF matches known vals.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "pmf")) {
        expect_false(is_intrinsic(d, "density"))
        p <- 1:99 / 100
        x <- unique(eval_quantile(d, at = p))
        pmf_evald <- eval_cdf(d, at = x) - eval_cdf(d, at = x - 1e-6)
        expect_equal(pmf_evald, eval_pmf(d, at = x))
      }
    }
  }
})
