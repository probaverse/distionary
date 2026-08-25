test_that("The cdf takes either inequality, and they differ on atoms", {
  d <- dst_pois(5)
  expect_equal(eval_cdf(d, at = 0:5), ppois(0:5, 5))
  expect_equal(eval_cdf(d, at = 0:5, inequality = "weak"), ppois(0:5, 5))
  expect_equal(
    eval_cdf(d, at = 0:5, inequality = "strict"),
    ppois(0:5, 5) - dpois(0:5, 5)
  )
})

test_that("The two inequalities agree without atoms", {
  d <- dst_norm(0, 1)
  expect_equal(
    eval_cdf(d, at = -2:2, inequality = "strict"),
    eval_cdf(d, at = -2:2)
  )
})

test_that("The inequality carries through to `enframe_cdf()`", {
  d <- dst_pois(5)
  expect_equal(
    enframe_cdf(d, at = 0:3, inequality = "strict")[["cdf"]],
    eval_cdf(d, at = 0:3, inequality = "strict")
  )
})
