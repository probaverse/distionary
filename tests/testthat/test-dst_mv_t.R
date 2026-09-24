test_that("the multivariate t matches mvtnorm", {
  skip_if_not_installed("mvtnorm")
  s <- matrix(c(1, 0.5, 0.5, 2), 2)
  d <- dst_mv_t(c(a = 0, b = 1), s, df = 3)
  expect_identical(pretty_name(d), "Bivariate Student t")
  expect_identical(vtype(d), "continuous")
  pt2 <- function(lower = -Inf, upper = Inf) {
    as.numeric(mvtnorm::pmvt(
      lower = lower,
      upper = upper,
      delta = c(0, 1),
      sigma = s,
      df = 3,
      type = "shifted",
      algorithm = mvtnorm::TVPACK()
    ))
  }
  expect_equal(eval_mv_cdf(d, list(0.3, 1.5)), pt2(upper = c(0.3, 1.5)))
  expect_equal(
    eval_mv_survival(d, list(0.3, 1.5)),
    pt2(lower = c(0.3, 1.5))
  )
  x <- rbind(c(0.3, 1.5), c(-1, 2))
  expect_equal(
    eval_mv_density(d, list(x[, 1], x[, 2])),
    mvtnorm::dmvt(x, delta = c(0, 1), sigma = s, df = 3, log = FALSE)
  )
})

test_that("a whole number of degrees of freedom is not needed", {
  skip_if_not_installed("mvtnorm")
  d <- dst_mv_t(c(0, 0), matrix(c(1, 0.5, 0.5, 2), 2), df = 2.5)
  set.seed(1)
  r <- realise(d, 2e5)
  expect_equal(
    eval_bi_cdf(d, 0.3, 0.5),
    mean(r$x1 <= 0.3 & r$x2 <= 0.5),
    tolerance = 0.01
  )
})

test_that("marginals and conditionals of the t are t", {
  skip_if_not_installed("mvtnorm")
  s <- matrix(c(1, 0.5, 0.5, 2), 2)
  d <- dst_mv_t(c(a = 0, b = 1), s, df = 3)
  b <- marginal(d, "b")
  expect_identical(pretty_name(b), "Student t")
  expect_equal(eval_cdf(b, 2), stats::pt((2 - 1) / sqrt(2), 3))
  # b | a = 1 is t with df 4, location 1.5, scale^2 (3 + 1) / 4 * 1.75.
  cb <- conditional(d, c(a = 1))
  expect_equal(parameters(cb)$df, 4)
  expect_equal(parameters(cb)$location, 1.5)
  expect_equal(parameters(cb)$scale, sqrt(1.75))
  expect_equal(
    eval_mv_cdf(d, list(1, 2), given = "a"),
    stats::pt((2 - 1.5) / sqrt(1.75), 4)
  )
  expect_equal(
    eval_mv_density(d, list(1, 2), given = "a"),
    stats::dt((2 - 1.5) / sqrt(1.75), 4) / sqrt(1.75)
  )
})

test_that("moments of the t exist only for enough degrees of freedom", {
  s <- matrix(c(1, 0.5, 0.5, 2), 2)
  expect_equal(mean(dst_mv_t(c(a = 0, b = 1), s, df = 3)), c(a = 0, b = 1))
  expect_equal(unname(variance(dst_mv_t(c(0, 1), s, df = 3))), s * 3)
  v <- variance(dst_mv_t(c(0, 1), s, df = 1.5))
  expect_identical(unname(diag(v)), c(Inf, Inf))
  expect_true(is.nan(v[1, 2]))
  expect_true(all(is.nan(mean(dst_mv_t(c(0, 1), s, df = 1)))))
})

test_that("slices of the t are exact", {
  skip_if_not_installed("mvtnorm")
  a <- rbind(r1 = c(1, 0), r2 = c(0, 1), s = c(1, 1))
  trio <- dst_mv_t(
    stats::setNames(as.numeric(a %*% c(50, 80)), rownames(a)),
    a %*% matrix(c(100, 50, 50, 200), 2) %*% t(a),
    df = 4
  )
  expect_identical(vtype(trio), "singular")
  sl <- conditional(trio, c(s = 200))
  expect_identical(parameters(sl)$df, 5)
  set.seed(2)
  expect_equal(unname(rowSums(realise(sl, 3))), rep(200, 3))
  expect_equal(
    eval_bi_cdf(sl, 80, 150),
    diff(eval_cdf(marginal(sl, "r1"), c(50, 80))),
    tolerance = 1e-6
  )
})

test_that("the t checks its parameters, and reduces where it should", {
  s <- diag(2)
  expect_error(dst_mv_t(c(0, 0), s, df = 0), "positive")
  expect_error(dst_mv_t(c(0, 0), diag(3), df = 2), "one row")
  expect_error(dst_mv_t(c(0, 0), matrix(c(1, 2, 2, 1), 2), 2), "semi")
  expect_true(is.na(dst_mv_t(c(0, NA), s, 2)))
  expect_identical(pretty_name(dst_mv_t(c(0, 0), s, Inf)), "Bivariate Normal")
  one <- dst_mv_t(3, matrix(4), 2)
  expect_identical(parameters(one), list(df = 2, location = 3, scale = 2))
  d <- dst_bi_t(c(0, 0), c(1, 2), 0.5, 3)
  expect_identical(variables(d), c("x", "y"))
  expect_identical(parameters(d)$cor, 0.5)
  expect_error(dst_bi_t(c(0, 0), c(1, 2), 1, 3), "strictly between")
})

test_that("dst_t() shifts and scales, and keeps the standard t as it was", {
  expect_identical(parameters(dst_t(3)), list(df = 3))
  d <- dst_t(3, location = 10, scale = 2)
  expect_equal(eval_cdf(d, 12), stats::pt(1, 3))
  expect_equal(eval_density(d, 12), stats::dt(1, 3) / 2)
  expect_equal(eval_quantile(d, 0.9), 10 + 2 * stats::qt(0.9, 3))
  expect_equal(mean(d), 10)
  expect_equal(variance(d), 4 * 3)
  expect_error(dst_t(3, location = 0, scale = 0), "positive")
  expect_error(dst_t(3, 1))
})

test_that("the vectorised bivariate Normal CDF matches mvtnorm", {
  skip_if_not_installed("mvtnorm")
  set.seed(1)
  for (rho in c(-0.92, -0.5, 0, 0.3, 0.9)) {
    h <- stats::rnorm(20, sd = 2)
    k <- stats::rnorm(20, sd = 2)
    truth <- vapply(seq_along(h), function(i) {
      as.numeric(mvtnorm::pmvnorm(
        upper = c(h[[i]], k[[i]]),
        corr = matrix(c(1, rho, rho, 1), 2),
        algorithm = mvtnorm::TVPACK(abseps = 1e-14)
      ))
    }, numeric(1))
    expect_equal(pbinorm(h, k, rho), truth, tolerance = 1e-12)
  }
  expect_equal(
    pbinorm(c(Inf, -Inf, 1, NA), c(0.5, 1, Inf, 1), 0.5),
    c(stats::pnorm(0.5), 0, stats::pnorm(1), NA)
  )
})
