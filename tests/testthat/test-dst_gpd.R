test_that("GPD quantities work out, shape > 0", {
  scale <- 1
  shape <- 1
  d <- dst_gpd(scale = scale, shape = shape)
  med <- scale * (2^shape - 1) / shape
  expect_equal(eval_cdf(d, c(-1, 0, med)), c(0, 0, 0.5))
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

test_that("cdf and pdf of GPD align via numerical derivative.", {
  d <- list(
    dst_gpd(1, 1),
    dst_gpd(1, 0),
    dst_gpd(10, -1)
  )
  x <- 1:11
  eps <- 1e-6
  for (i in seq_along(d)) {
    pdf <- eval_density(d[[i]], at = x)
    cdf1 <- eval_cdf(d[[i]], at = x - eps)
    cdf2 <- eval_cdf(d[[i]], at = x)
    pdf_num <- (cdf2 - cdf1) / eps
    expect_equal(pdf, pdf_num, tolerance = 1e-6)
  }
})

test_that("cdf and qf of GPD align.", {
  d <- list(
    dst_gpd(1, 1),
    dst_gpd(1, 0),
    dst_gpd(10, -1)
  )
  p <- 1:9 / 10
  for (i in seq_along(d)) {
    qf <- eval_quantile(d[[i]], at = p)
    cdf <- eval_cdf(d[[i]], at = qf)
    expect_equal(cdf, p)
  }
})

test_that("quantile function of GPD is valid, validating the distribution.", {
  d <- list(
    dst_gpd(1, 1),
    dst_gpd(1, 0),
    dst_gpd(10, -1)
  )
  p <- 0:100 / 100
  for (i in seq_along(d)) {
    r <- range(d[[i]])
    qf <- eval_quantile(d[[i]], at = p)
    expect_true(all(diff(qf) > 0))
    expect_equal(qf[c(1L, 101L)], r)
  }
})

