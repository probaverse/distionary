test_that("Built-in density functions match cdf", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "density")) {
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
