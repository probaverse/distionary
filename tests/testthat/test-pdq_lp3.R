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

test_that("LP3 negative scale and skew not allowed.", {
  expect_error(dlp3(1:10, meanlog = 0, sdlog = -4, skew = 1))
  expect_error(plp3(1:10, meanlog = 0, sdlog = -4, skew = 1))
  expect_error(qlp3(1:10 / 10, meanlog = 0, sdlog = -4, skew = 1))
  expect_error(dlp3(1:10, meanlog = 0, sdlog = 4, skew = -1))
  expect_error(plp3(1:10, meanlog = 0, sdlog = 4, skew = -1))
  expect_error(qlp3(1:10 / 10, meanlog = 0, sdlog = 4, skew = -1))
})

test_that("cdf and pdf align via numerical derivative.", {
  d <- list(
    dst_lp3(0, 1, 1),
    dst_lp3(0, 1, 0),
    dst_lp3(0, 10, 0.5)
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

test_that("recycling not allowed in random number generator.", {
  expect_error(rlp3(10, meanlog = 1:2, sdlog = 1, skew = 1))
  expect_error(rlp3(10, meanlog = 1, sdlog = 1:2, skew = 1))
  expect_error(rlp3(10, meanlog = 1, sdlog = 1, skew = 1:2))
})

test_that("cdf and qf align.", {
  d <- list(
    dst_lp3(0, 1, 1),
    dst_lp3(0, 1, 0.1),
    dst_lp3(0, 10, 0.5)
  )
  p <- 1:9 / 10
  for (i in seq_along(d)) {
    qf <- eval_quantile(d[[i]], at = p)
    cdf <- eval_cdf(d[[i]], at = qf)
    expect_equal(cdf, p)
  }
})

test_that("quantile function of LP3 is valid, validating the distribution.", {
  d <- list(
    list(meanlog = 0, sdlog = 1.2, skew = 1.1),
    list(meanlog = 0, sdlog = 1.2, skew = 0.1),
    list(meanlog = 0, sdlog = 10, skew = 0.5)
  )
  p <- 0:100 / 100
  for (i in seq_along(d)) {
    qf <- rlang::exec(qlp3, p, !!!d[[i]])
    expect_true(all(diff(qf) > 0))
    expect_equal(qf[101L], Inf)
  }
})

test_that("vectorisation of p/d/q/r functions works.", {
  y <- 1:10
  x <- 1:10
  x[4] <- NA_real_
  x[2] <- NaN
  p <- x / 11
  v <- y / 11
  # Mismatched lengths
  expect_error(plp3(x, 1:2, 1, 1))
  expect_error(plp3(x, 1, 0:1, 1))
  expect_error(plp3(x, 1, 1, 1:2))
  expect_error(dlp3(x, 1:2, 1, 1))
  expect_error(dlp3(x, 1, 0:1, 1))
  expect_error(dlp3(x, 1, 1, 1:2))
  expect_error(qlp3(p, 1:2, 1, 1))
  expect_error(qlp3(p, 1, 0:1, 1))
  expect_error(qlp3(p, 1, 1, 1:2))
  # Lengths input correctly; should be length 10.
  expect_length(plp3(x, 1, 1, 1), 10)
  expect_length(dlp3(x, 1, 1, 1), 10)
  expect_length(qlp3(p, 1, 1, 1), 10)
  expect_length(plp3(x, 1:10, 1, 1), 10)
  expect_length(dlp3(x, 1:10, 1, 1), 10)
  expect_length(qlp3(p, 1:10, 1, 1), 10)
  # NA and NaN gets projected forward. NaN may convert to NA.
  # --> main argument
  expect_true(is.na(plp3(x, 1, 1, 1)[4]))
  expect_true(is.na(dlp3(x, 1, 1, 1)[4]))
  expect_true(is.na(qlp3(p, 1, 1, 1)[4]))
  expect_true(is.na(plp3(x, 1, 1, 1)[2]))
  expect_true(is.na(dlp3(x, 1, 1, 1)[2]))
  expect_true(is.na(qlp3(p, 1, 1, 1)[2]))
  # --> meanlong
  expect_true(is.na(plp3(y, x, 1, 1)[4]))
  expect_true(is.na(dlp3(y, x, 1, 1)[4]))
  expect_true(is.na(qlp3(v, x, 1, 1)[4]))
  expect_true(is.na(plp3(y, x, 1, 1)[2]))
  expect_true(is.na(dlp3(y, x, 1, 1)[2]))
  expect_true(is.na(qlp3(v, x, 1, 1)[2]))
  # --> sdlog
  expect_true(is.na(plp3(y, 1, x, 1)[4]))
  expect_true(is.na(dlp3(y, 1, x, 1)[4]))
  expect_true(is.na(qlp3(v, 1, x, 1)[4]))
  expect_true(is.na(plp3(y, 1, x, 1)[2]))
  expect_true(is.na(dlp3(y, 1, x, 1)[2]))
  expect_true(is.na(qlp3(v, 1, x, 1)[2]))
  # --> skew
  expect_true(is.na(plp3(y, 1, 1, x)[4]))
  expect_true(is.na(dlp3(y, 1, 1, x)[4]))
  expect_true(is.na(qlp3(v, 1, 1, x)[4]))
  expect_true(is.na(plp3(y, 1, 1, x)[2]))
  expect_true(is.na(dlp3(y, 1, 1, x)[2]))
  expect_true(is.na(qlp3(v, 1, 1, x)[2]))
  # random generator
  expect_error(rlp3(10, 1:2, 1, 1))
  expect_error(rlp3(10, 1, 0:1, 1))
  expect_error(rlp3(10, 1, 1, 1:2))
  expect_error(rlp3(1:10, 1, 1, 1))
})

