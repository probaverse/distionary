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

#' CHF is never intrinsic, which means there are no known values to compare
#' against. Compare built-in algorithm with another algorithm for deriving
#' CHF: integrating the hazard.
#' This also checks that the hazard function is correct.
test_that("CHF algorithm validated by comparing to a different algorithm.", {
  # - `eval_chf()` goes through the survival function only.
  # - integrating the hazard goes through both the density and survival.
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (vtype(d) == "continuous") {
        p <- 1:9 / 10
        x <- eval_quantile(d, at = p)
        haz_fun <- representation_as_function(d, "hazard")
        increments <- vapply(
          seq_along(x[-1L]),
          function(t) {
            stats::integrate(haz_fun, x[1], x[t + 1])$value
          },
          FUN.VALUE = numeric(1L)
        )
        chf_derived <- eval_chf(d, at = x[1]) + append(0, (increments))
        expect_equal(chf_derived, eval_chf(d, at = x))
      }
    }
  }
})
