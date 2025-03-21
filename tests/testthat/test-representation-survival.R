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
