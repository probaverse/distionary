#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters.

test_that("Degenerate Distribution quantities are appropriate", {
  d <- dst_degenerate(0.5)
  expect_equal(eval_pmf(d, -10:10), rep(0, 21))
  expect_equal(eval_pmf(d, 0.5), 1)
  expect_equal(eval_cdf(d, -10:10), c(rep(0, 11), rep(1, 10)))
  expect_equal(eval_cdf(d, 0.5), 1)
  expect_equal(eval_survival(d, c(0, 0.5, 1)), c(1, 0, 0))
  expect_equal(realise(d, 10), rep(0.5, 10))
  expect_equal(eval_quantile(d, -1:11 / 10), c(NaN, rep(0.5, 11), NaN))
  expect_equal(mean(d), 0.5)
  expect_equal(variance(d), 0)
  expect_equal(skewness(d), NaN)
  expect_equal(kurtosis_exc(d), NaN)
  expect_equal(range(d), c(0.5, 0.5))
})
