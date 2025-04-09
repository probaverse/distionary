#' @srrstats {G3.0} Appropriate tolerances for approximate equality is
#' adopted in instances of `expect_equal()`. The default is used, except
#' for instances where comparison can allow a larger tolerance. --> This
#' srrstats statement is included in all test files that use a different
#' tolerance in `expect_equal()` than the default.
#' @srrstats {G5.6} Parameter recovery is relevant when distributional
#' properties (like quantiles) are computed from other properties (like the
#' cdf); these are possible to validate by comparing the intrinsic version
#' of the property against the derived version as if it were absent. See,
#' for example, `test-representation-density.R` (copied there).
#' @srrstats {G5.6a} Parameter recovery tests are conducted using the default
#' tolerance in the `testthat::expect_equal()` function whenever tests of
#' user-facing outputs are evaluated.


test_that("Built-in density functions match cdf", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "density")) {
        expect_false(is_intrinsic(d, "pmf"))
        rng <- range(d)
        p <- 1:99 / 100
        x <- eval_quantile(d, at = p)
        dens_fun <- representation_as_function(d, "density")
        increments <- vapply(
          seq_along(x[-1L]),
          function(t) {
            stats::integrate(dens_fun, lower = x[t], upper = x[t + 1])$value
          },
          FUN.VALUE = numeric(1L)
        )
        cdf_derived <- p[1] + append(0, cumsum(increments))
        expect_equal(cdf_derived, eval_cdf(d, at = x))
      }
    }
  }
})
