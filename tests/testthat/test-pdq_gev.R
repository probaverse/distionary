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

test_that("GEV negative scale not allowed.", {
  expect_error(dgev(1:10, 3, -4, 1))
  expect_error(pgev(1:10, 3, -4, 1))
  expect_error(qgev(1:10 / 10, 3, -4, 1))
})

test_that("cdf and pdf of GEV align via numerical derivative.", {
  d <- list(
    list(location = 0.2, scale = 1.2, shape = 0.8),
    list(location = 0.2, scale = 1.2, shape = 0),
    list(location = 0.2, scale = 1.2, shape = -0.8)
  )
  x <- -5:5
  eps <- 1e-6
  for (i in seq_along(d)) {
    pdf <- rlang::exec(dgev, x, !!!d[[i]])
    cdf1 <- rlang::exec(pgev, x - eps, !!!d[[i]])
    cdf2 <- rlang::exec(pgev, x, !!!d[[i]])
    pdf_num <- (cdf2 - cdf1) / eps
    expect_equal(pdf, pdf_num, tolerance = 1e-6)
  }
})

test_that("cdf and qf of GEV align.", {
  d <- list(
    list(location = 0.2, scale = 1.2, shape = 0.8),
    list(location = 0.2, scale = 1.2, shape = 0),
    list(location = 0.2, scale = 1.2, shape = -0.8)
  )
  p <- 1:9 / 10
  for (i in seq_along(d)) {
    qf <- rlang::exec(qgev, p, !!!d[[i]])
    cdf <- rlang::exec(pgev, qf, !!!d[[i]])
    expect_equal(cdf, p)
  }
})

test_that("quantile function of GEV is valid, validating the distribution.", {
  d <- list(
    list(location = 0.2, scale = 1.2, shape = 0.8),
    list(location = 0.2, scale = 1.2, shape = 0),
    list(location = 0.2, scale = 1.2, shape = -0.8)
  )
  p <- 0:100 / 100
  for (i in seq_along(d)) {
    lo <- rlang::exec(distionary:::gev_lower, !!!d[[i]])
    hi <- rlang::exec(distionary:::gev_upper, !!!d[[i]])
    r <- c(lo, hi)
    qf <- rlang::exec(qgev, p, !!!d[[i]])
    expect_true(all(diff(qf) > 0))
    expect_equal(min(qf), lo)
    expect_equal(max(qf), hi)
  }
})

test_that("vectorisation of p/d/q GEV functions works.", {
  y <- 1:10
  x <- 1:10
  x[4] <- NA_real_
  x[2] <- NaN
  p <- x / 11
  v <- y / 11
  # Mismatched lengths
  expect_error(pgev(x, 0:1, 1, 1))
  expect_error(pgev(x, 0, 1:2, 1))
  expect_error(pgev(x, 0, 1, 0:1))
  expect_error(dgev(x, 0:1, 1, 1))
  expect_error(dgev(x, 0, 1:2, 1))
  expect_error(dgev(x, 0, 1, 0:1))
  expect_error(qgev(p, 0:1, 1, 1))
  expect_error(qgev(p, 0, 1:2, 1))
  expect_error(qgev(p, 0, 1, 0:1))
  expect_error(distionary:::gev_lower(0:10, 1:2, 1))
  expect_error(distionary:::gev_lower(1, 1:2, 1:4))
  expect_error(distionary:::gev_lower(0:10, 2, 1:3))
  expect_error(distionary:::gev_upper(0:10, 1:2, 1))
  expect_error(distionary:::gev_upper(1, 1:2, 1:4))
  expect_error(distionary:::gev_upper(0:10, 2, 1:3))
  # Lengths input correctly; should be length 10.
  expect_length(pgev(x, 0, 1, 1), 10)
  expect_length(dgev(x, 0, 1, 1), 10)
  expect_length(qgev(p, 0, 1, 1), 10)
  expect_length(pgev(x, 0, 1:10, 1), 10)
  expect_length(dgev(x, 0, 1:10, 1), 10)
  expect_length(qgev(p, 0, 1:10, 1), 10)
  # NA and NaN gets projected forward. NaN may convert to NA.
  # --> main argument
  expect_true(is.na(pgev(x, 0, 1, 1)[4]))
  expect_true(is.na(dgev(x, 0, 1, 1)[4]))
  expect_true(is.na(qgev(p, 0, 1, 1)[4]))
  expect_true(is.na(pgev(x, 0, 1, 1)[2]))
  expect_true(is.na(dgev(x, 0, 1, 1)[2]))
  expect_true(is.na(qgev(p, 0, 1, 1)[2]))
  # --> location
  expect_true(is.na(pgev(y, x, 1, 0)[4]))
  expect_true(is.na(dgev(y, x, 1, 0)[4]))
  expect_true(is.na(qgev(v, x, 1, 0)[4]))
  expect_true(is.na(pgev(y, x, 1, 0)[2]))
  expect_true(is.na(dgev(y, x, 1, 0)[2]))
  expect_true(is.na(qgev(v, x, 1, 0)[2]))
  # --> scale
  expect_true(is.na(pgev(y, 0, x, 1)[4]))
  expect_true(is.na(dgev(y, 0, x, 1)[4]))
  expect_true(is.na(qgev(v, 0, x, 1)[4]))
  expect_true(is.na(pgev(y, 0, x, 1)[2]))
  expect_true(is.na(dgev(y, 0, x, 1)[2]))
  expect_true(is.na(qgev(v, 0, x, 1)[2]))
  # --> shape
  expect_true(is.na(pgev(y, 0, 1, x)[4]))
  expect_true(is.na(dgev(y, 0, 1, x)[4]))
  expect_true(is.na(qgev(v, 0, 1, x)[4]))
  expect_true(is.na(pgev(y, 0, 1, x)[2]))
  expect_true(is.na(dgev(y, 0, 1, x)[2]))
  expect_true(is.na(qgev(v, 0, 1, x)[2]))
  # Min and Max
  expect_length(distionary:::gev_lower(x, 1, 1), 10)
  expect_length(distionary:::gev_lower(1, x, 1), 10)
  expect_length(distionary:::gev_lower(1, 1, x), 10)
  expect_length(distionary:::gev_upper(x, 1, 1), 10)
  expect_length(distionary:::gev_upper(1, x, 1), 10)
  expect_length(distionary:::gev_upper(1, 1, x), 10)
})
