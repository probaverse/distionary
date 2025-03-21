test_that("Odds matches some known values", {
  expect_equal(eval_odds(dst_bern(0.5), at = 1), 1)
  expect_equal(eval_odds(dst_bern(0.2), at = 1), 0.25)
  expect_equal(eval_odds(dst_bern(0.8), at = 1), 4)
  d <- dst_pois(4)
  expect_equal(
    eval_odds(d, at = 0:10),
    eval_pmf(d, at = 0:10) / (1 - eval_pmf(d, at = 0:10))
  )
})
