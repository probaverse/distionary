
test_that("Return levels match some known values", {
  expect_equal(eval_return(dst_unif(0, 1), at = 1), 0)
  expect_equal(eval_return(dst_unif(0, 1), at = 2), 0.5)
  expect_equal(eval_return(dst_unif(0, 1), at = 10), 0.9)
  expect_equal(eval_return(dst_unif(0, 1), at = Inf), 1)
  expect_equal(
    suppressWarnings(eval_return(dst_unif(0, 1), at = c(-1, 0.5))),
    c(NaN, NaN)
  )
})


test_that("Return levels can be taken from either tail", {
  d <- dst_norm(0, 1)
  expect_equal(eval_return(d, at = 100), eval_quantile(d, at = 0.99))
  expect_equal(
    eval_return(d, at = 100, event = "lower"),
    eval_quantile(d, at = 0.01)
  )
  # The two tails of a symmetric distribution mirror one another.
  expect_equal(
    eval_return(d, at = c(2, 25, 100), event = "lower"),
    -eval_return(d, at = c(2, 25, 100))
  )
})

test_that("Return periods can be quoted in periods of many observations", {
  d <- dst_gp(24, 0.3)
  expect_equal(
    eval_return(d, at = 2, obs_per_period = 50),
    eval_return(d, at = 100)
  )
  expect_equal(
    eval_return(d, at = c(1, 10), obs_per_period = 365),
    eval_return(d, at = c(365, 3650))
  )
  # The default is one observation per period: the scale it always used.
  expect_equal(
    eval_return(d, at = c(2, 25), obs_per_period = 1),
    eval_return(d, at = c(2, 25))
  )
})

test_that("`obs_per_period` has to be a positive number", {
  d <- dst_norm(0, 1)
  expect_error(eval_return(d, at = 2, obs_per_period = 0), "has to be")
  expect_error(eval_return(d, at = 2, obs_per_period = -5), "has to be")
  expect_error(eval_return(d, at = 2, obs_per_period = c(1, 2)))
})

test_that("Return variants carry through to `enframe_return()`", {
  d <- dst_norm(0, 1)
  expect_equal(
    enframe_return(d, at = c(2, 25), event = "lower")[["return"]],
    eval_return(d, at = c(2, 25), event = "lower")
  )
  expect_equal(
    enframe_return(d, at = 2, obs_per_period = 50)[["return"]],
    eval_return(d, at = 100)
  )
})
