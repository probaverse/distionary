# Validate PMF can be recovered from CDF
#
# Systematic testing uses the `test_distributions` object.
# To modify it, see `data-raw/test_distributions.R`.
# To update it, run `Rscript data-raw/test_distributions.R`.


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
