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
