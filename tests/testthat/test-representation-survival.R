# Validates survival can be recovered from network
#
# Systematic testing uses the `test_distributions` object.
# To modify it, see `data-raw/test_distributions.R`.
# To update it, run `Rscript data-raw/test_distributions.R`.


test_that("Survival function calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      p <- 0:100 / 100
      x <- eval_quantile(d, at = p)
      if (is_intrinsic(d, "survival")) {
        expect_equal(
          eval_survival_from_network(d, at = x),
          eval_survival(d, at = x)
        )
      }
    }
  }
})
