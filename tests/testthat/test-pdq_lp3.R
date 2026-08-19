lp3_ref_cdf <- function(x, meanlog, sdlog, skew) {
  logx <- log(x)
  if (skew == 0) {
    return(stats::pnorm(logx, meanlog, sdlog))
  }
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  if (skew > 0) {
    shift <- meanlog - scale * shape
    stats::pgamma(logx - shift, shape = shape, scale = scale)
  } else {
    shift <- meanlog + scale * shape
    stats::pgamma(shift - logx, shape = shape, scale = scale, lower.tail = FALSE)
  }
}

lp3_ref_quantile <- function(p, meanlog, sdlog, skew) {
  if (skew == 0) {
    return(exp(stats::qnorm(p, meanlog, sdlog)))
  }
  shape <- 4 / skew^2
  scale <- sdlog / sqrt(shape)
  if (skew > 0) {
    shift <- meanlog - scale * shape
    exp(stats::qgamma(p, shape = shape, scale = scale) + shift)
  } else {
    shift <- meanlog + scale * shape
    exp(shift - stats::qgamma(1 - p, shape = shape, scale = scale))
  }
}

test_that("LP3 negative sdlog not allowed.", {
  expect_error(dlp3(1:10, meanlog = 0, sdlog = -4, skew = 1))
  expect_error(plp3(1:10, meanlog = 0, sdlog = -4, skew = 1))
  expect_error(qlp3(1:10 / 10, meanlog = 0, sdlog = -4, skew = 1))
})

test_that("LP3 matches signed-skew reference for cdf and quantile.", {
  cases <- list(
    list(meanlog = 0, sdlog = 1, skew = 0.5),
    list(meanlog = -0.3, sdlog = 1.2, skew = 1.1),
    list(meanlog = 0.2, sdlog = 0.9, skew = -0.5),
    list(meanlog = -0.5, sdlog = 1.4, skew = -1.2),
    list(meanlog = 0, sdlog = 1, skew = 0)
  )
  x <- c(0.25, 0.7, 1, 2.5, 8)
  p <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  for (case in cases) {
    d <- rlang::exec(dst_lp3, !!!case)
    expect_equal(parameters(d), case)
    expect_equal(
      eval_cdf(d, at = x),
      lp3_ref_cdf(x, case$meanlog, case$sdlog, case$skew),
      tolerance = 1e-6
    )
    expect_equal(
      eval_quantile(d, at = p),
      lp3_ref_quantile(p, case$meanlog, case$sdlog, case$skew),
      tolerance = 1e-6
    )
    expect_equal(
      plp3(x, !!!case),
      lp3_ref_cdf(x, case$meanlog, case$sdlog, case$skew),
      tolerance = 1e-6
    )
    expect_equal(
      qlp3(p, !!!case),
      lp3_ref_quantile(p, case$meanlog, case$sdlog, case$skew),
      tolerance = 1e-6
    )
  }
})

test_that("cdf and pdf align via numerical derivative.", {
  d <- list(
    dst_lp3(0, 1, 1),
    dst_lp3(0, 1, 0),
    dst_lp3(0, 10, 0.5),
    dst_lp3(0, 1.2, -0.6)
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
    dst_lp3(0, 10, 0.5),
    dst_lp3(-0.2, 1.5, -0.8)
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
    list(meanlog = 0, sdlog = 10, skew = 0.5),
    list(meanlog = 0, sdlog = 1.2, skew = -0.5)
  )
  p <- 0:100 / 100
  for (i in seq_along(d)) {
    params <- d[[i]]
    qf <- rlang::exec(qlp3, p, !!!params)
    expect_true(all(diff(qf) > 0))
    if (params$skew >= 0) {
      expect_equal(qf[101L], Inf)
    } else {
      shape <- 4 / params$skew^2
      scale <- params$sdlog / sqrt(shape)
      shift_upper <- params$meanlog + scale * shape
      expect_equal(qf[101L], exp(shift_upper))
    }
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

