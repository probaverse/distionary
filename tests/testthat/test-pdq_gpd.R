#' @srrstats {PD4.0} The numeric outputs of probability distribution
#' functions are rigorously tested, not just output structures. These
#' tests are for numeric equality.
#' @srrstats {PD4.1} Tests for numeric equality compare the output of
#' probability distribution functions with the output of code defined
#' in the same location in test files.
#' @srrstats {G3.0} Appropriate tolerances for approximate equality is
#' adopted in instances of `expect_equal()`. The default is used, except
#' for instances where comparison can allow a larger tolerance. --> This
#' srrstats statement is included in all test files that use a different
#' tolerance in `expect_equal()` than the default.

test_that("GP quantities match known cases, shape > 0", {
  scale <- 1
  shape <- 1
  med <- scale * (2^shape - 1) / shape
  expect_equal(
    pgp(c(-1, 0, med), scale = scale, shape = shape), c(0, 0, 0.5)
  )
  expect_equal(
    qgp(c(-1, 0, 0.5, 1, 2), scale = scale, shape = shape),
    c(NaN, 0, med, Inf, NaN)
  )
  expect_equal(dgp(c(-1, Inf), scale = scale, shape = shape), c(0, 0))
  expect_true(all(diff(dgp(1:10, scale = scale, shape = shape)) < 0))
  pdf <- function(x) {
    dgp(x, scale = scale, shape = shape)
  }
  expect_equal(stats::integrate(pdf, 0, Inf)$value, 1, tolerance = 0.0001)
})


test_that("GP quantities match known cases, shape = 0", {
  scale <- 1
  shape <- 0
  med <- log(2)
  expect_equal(
    pgp(c(-1, 0, med, Inf), scale = scale, shape = shape), c(0, 0, 0.5, 1)
  )
  expect_equal(
    suppressWarnings(qgp(c(-1, 0, 0.5, 1, 2), scale = scale, shape = shape)),
    c(NaN, 0, med, Inf, NaN)
  )
  expect_equal(dgp(c(-1, Inf), scale = scale, shape = shape), c(0, 0))
  pdf <- function(x) {
    dgp(x, scale = scale, shape = shape)
  }
  expect_true(all(diff(pdf(1:10)) < 0))
  expect_equal(stats::integrate(pdf, 0, Inf)$value, 1, tolerance = 0.0001)
})

test_that("GP quantities match known cases, shape < 0", {
  scale <- 2
  shape <- -1
  rightend <- -scale / shape
  med <- scale * (2^shape - 1) / shape
  expect_equal(
    pgp(
      c(-1, 0, med, rightend, rightend + 1, Inf),
      scale = scale, shape = shape
    ),
    c(0, 0, 0.5, 1, 1, 1)
  )
  expect_equal(
    qgp(c(-1, 0, 0.5, 1, 2), scale = scale, shape = shape),
    c(NaN, 0, med, rightend, NaN)
  )
  expect_equal(
    dgp(c(-1, rightend + 1, Inf), scale = scale, shape = shape),
    c(0, 0, 0)
  )
  pdf <- function(x) {
    dgp(x, scale = scale, shape = shape)
  }
  expect_equal(stats::integrate(pdf, 0, rightend)$value, 1, tolerance = 0.0001)
})

test_that("GP negative scale not allowed.", {
  expect_error(dgev(1:10, -4, 1))
  expect_error(pgev(1:10, -4, 1))
  expect_error(qgev(1:10 / 10, -4, 1))
})

test_that("cdf and pdf of GP align via numerical derivative.", {
  d <- list(
    dst_gp(1, 1),
    dst_gp(1, 0),
    dst_gp(10, -1)
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

test_that("cdf and qf of GP align.", {
  d <- list(
    dst_gp(1, 1),
    dst_gp(1, 0),
    dst_gp(10, -1)
  )
  p <- 1:9 / 10
  for (i in seq_along(d)) {
    qf <- eval_quantile(d[[i]], at = p)
    cdf <- eval_cdf(d[[i]], at = qf)
    expect_equal(cdf, p)
  }
})

test_that("quantile function of GP is valid, validating the distribution.", {
  d <- list(
    list(scale = 1.2, shape = 1),
    list(scale = 1.2, shape = 0),
    list(scale = 10, shape = -1)
  )
  p <- 0:100 / 100
  for (i in seq_along(d)) {
    hi <- rlang::exec(distionary:::gp_upper, !!!d[[i]])
    qf <- rlang::exec(qgp, p, !!!d[[i]])
    expect_true(all(diff(qf) > 0))
    expect_equal(qf[c(1L, 101L)], c(0, hi))
  }
})

test_that("vectorisation of p/d/q GP functions works.", {
  y <- 1:10
  x <- 1:10
  x[4] <- NA_real_
  x[2] <- NaN
  p <- x / 11
  v <- y / 11
  # Mismatched lengths
  expect_error(pgp(x, 1:2, 1))
  expect_error(pgp(x, 1, 0:1))
  expect_error(dgp(x, 1:2, 1))
  expect_error(dgp(x, 1, 0:1))
  expect_error(qgp(p, 1:2, 1))
  expect_error(qgp(p, 1, 0:1))
  expect_error(distionary:::gp_lower(1:2, 1:4))
  expect_error(distionary:::gp_upper(1:2, 1:4))
  # Lengths input correctly; should be length 10.
  expect_length(pgp(x, 1, 1), 10)
  expect_length(dgp(x, 1, 1), 10)
  expect_length(qgp(p, 1, 1), 10)
  expect_length(pgp(x, 1:10, 1), 10)
  expect_length(dgp(x, 1:10, 1), 10)
  expect_length(qgp(p, 1:10, 1), 10)
  # NA and NaN gets projected forward. NaN may convert to NA.
  # --> main argument
  expect_true(is.na(pgp(x, 1, 1)[4]))
  expect_true(is.na(dgp(x, 1, 1)[4]))
  expect_true(is.na(qgp(p, 1, 1)[4]))
  expect_true(is.na(pgp(x, 1, 1)[2]))
  expect_true(is.na(dgp(x, 1, 1)[2]))
  expect_true(is.na(qgp(p, 1, 1)[2]))
  # --> scale
  expect_true(is.na(pgp(y, x, 1)[4]))
  expect_true(is.na(dgp(y, x, 1)[4]))
  expect_true(is.na(qgp(v, x, 1)[4]))
  expect_true(is.na(pgp(y, x, 1)[2]))
  expect_true(is.na(dgp(y, x, 1)[2]))
  expect_true(is.na(qgp(v, x, 1)[2]))
  # --> shape
  expect_true(is.na(pgp(y, 1, x)[4]))
  expect_true(is.na(dgp(y, 1, x)[4]))
  expect_true(is.na(qgp(v, 1, x)[4]))
  expect_true(is.na(pgp(y, 1, x)[2]))
  expect_true(is.na(dgp(y, 1, x)[2]))
  expect_true(is.na(qgp(v, 1, x)[2]))
  # Max
  expect_length(distionary:::gp_upper(x, 1), 10)
  expect_length(distionary:::gp_upper(1, x), 10)
})

