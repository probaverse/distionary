test_that("GPD quantities work out, shape > 0", {
  scale <- 1
  shape <- 1
  d <- dst_gpd(scale = scale, shape = shape)
  med <- scale * (2^shape - 1) / shape
  expect_equal(eval_cdf(d, c(- 1, 0, med)), c(0, 0, 0.5))
  expect_equal(eval_quantile(d, c(0, 0.5, 1)), c(0, med, Inf))
  expect_equal(eval_density(d, c(-1, Inf)), c(0, 0))
  expect_true(all(diff(eval_density(d, 1:10)) < 0))
  pdf <- representation_as_function(d, "density")
  expect_equal(integrate(pdf, 0, Inf)$value, 1, tolerance = 0.0001)
  expect_true(all(realise(d, 10) >= 0))
})


test_that("GPD quantities work out, shape = 0", {
  scale <- 1
  shape <- 0
  d <- dst_gpd(scale = scale, shape = shape)
  med <- log(2)
  expect_equal(eval_cdf(d, c(-1, 0, med, Inf)), c(0, 0, 0.5, 1))
  expect_equal(eval_quantile(d, c(0, 0.5, 1)), c(0, med, Inf))
  expect_equal(eval_density(d, c(-1, Inf)), c(0, 0))
  pdf <- representation_as_function(d, "density")
  expect_true(all(diff(pdf(1:10)) < 0))
  expect_equal(integrate(pdf, 0, Inf)$value, 1, tolerance = 0.0001)
  expect_true(all(realise(d, 10) >= 0))
})

test_that("GPD quantities work out, shape < 0", {
  scale <- 2
  shape <- -1
  rightend <- -scale / shape
  d <- dst_gpd(scale = scale, shape = shape)
  med <- scale * (2^shape - 1) / shape
  expect_equal(
    eval_cdf(d, c(-1, 0, med, rightend, rightend + 1, Inf)),
    c(0, 0, 0.5, 1, 1, 1)
  )
  expect_equal(
    eval_quantile(d, c(-1, 0, 0.5, 1, 2)),
    c(NaN, 0, med, rightend, NaN)
  )
  expect_equal(
    eval_density(d, c(-1, rightend + 1, Inf)),
    c(0, 0, 0)
  )
  pdf <- representation_as_function(d, "density")
  expect_equal(integrate(pdf, 0, rightend)$value, 1, tolerance = 0.0001)
  r <- realise(d, 10)
  expect_true(all(r > 0 & r < rightend))
})

test_that("GPD parameter restrictions enforced", {
  expect_error(dst_gpd(scale = -1, shape = 1))
})
