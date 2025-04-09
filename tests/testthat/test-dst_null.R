#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters.

test_that("Null distribution works", {
  d <- dst_null()
  expect_equal(mean(d), NA_real_)
  expect_equal(variance(d), NA_real_)
  expect_equal(stdev(d), NA_real_)
  expect_equal(skewness(d), NA_real_)
  expect_equal(kurtosis(d), NA_real_)
  expect_equal(kurtosis_exc(d), NA_real_)
  expect_equal(eval_cdf(d, at = 0), NA_real_)
  expect_equal(eval_density(d, at = 0), NA_real_)
  expect_equal(eval_pmf(d, at = 0), NA_real_)
  expect_equal(eval_quantile(d, at = 0), NA_real_)
  expect_equal(realise(d), NA_real_)
  expect_equal(vtype(d), NA_character_)
})
