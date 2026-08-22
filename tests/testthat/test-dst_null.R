
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


test_that("The Null distribution answers `NA` to every variant", {
  d <- dst_null()
  expect_equal(eval_quantile(d, at = 0.5, side = "right"), NA_real_)
  expect_equal(eval_cdf(d, at = 1, inequality = "strict"), NA_real_)
  expect_equal(eval_survival(d, at = 1, inequality = "weak"), NA_real_)
  expect_equal(eval_density(d, at = 1, definition = "strict"), NA_real_)
  expect_equal(eval_pmf(d, at = 1, definition = "strict"), NA_real_)
  expect_equal(eval_return(d, at = 100, event = "lower"), NA_real_)
  expect_equal(eval_prob_left(d, at = 1, inequality = "strict"), NA_real_)
  expect_equal(eval_prob_right(d, at = 1, inequality = "weak"), NA_real_)
})
