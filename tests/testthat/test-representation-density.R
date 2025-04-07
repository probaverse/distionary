#' @srrstats {G5.6} Parameter recovery is relevant when distributional
#' properties (like quantiles) are computed from other properties (like the
#' cdf); these are possible to validate by comparing the intrinsic version
#' of the property against the derived version as if it were absent. See,
#' for example, `test-representation-density.R` (copied there).

test_that("Built-in density functions match cdf", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "density")) {
        expect_false(is_intrinsic("pmf"))
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
