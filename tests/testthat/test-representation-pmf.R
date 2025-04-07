test_that("PMF calculated thru CDF matches known vals.", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "pmf")) {
        expect_false(is_intrinsic("density"))
        p <- 1:99 / 100
        x <- unique(eval_quantile(d, at = p))
        pmf_evald <- eval_cdf(d, at = x) - eval_cdf(d, at = x - 1)
        expect_equal(pmf_evald, eval_pmf(d, at = x))
      }
    }
  }
})
