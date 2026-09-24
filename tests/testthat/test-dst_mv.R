test_that("the bivariate Normal matches known values", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  expect_identical(pretty_name(d), "Bivariate Normal")
  expect_identical(vtype(d), "continuous")
  expect_identical(parameters(d)$cor, 0.6)
  # Orthant probability of a standard bivariate Normal at its mean.
  expect_equal(eval_bi_cdf(d, 0, 1), 0.25 + asin(0.6) / (2 * pi))
  expect_equal(eval_bi_survival(d, 0, 1), 0.25 + asin(0.6) / (2 * pi))
  # Density against the closed form.
  x <- c(-1, 0.5)
  y <- c(2, -1)
  zx <- x
  zy <- (y - 1) / 2
  q <- (zx^2 - 2 * 0.6 * zx * zy + zy^2) / (1 - 0.36)
  expect_equal(
    eval_bi_density(d, x, y),
    exp(-q / 2) / (2 * pi * 2 * sqrt(1 - 0.36))
  )
  expect_equal(mean(d), c(x = 0, y = 1))
  expect_equal(unname(variance(d)), matrix(c(1, 1.2, 1.2, 4), 2))
  expect_equal(stdev(d), c(x = 1, y = 2))
  expect_equal(eval_bi_cdf(d, Inf, 1), 0.5)
  expect_equal(eval_bi_cdf(d, -Inf, 1), 0)
})

test_that("the multivariate Normal's CDF matches a known value", {
  skip_if_not_installed("mvtnorm")
  rho <- 0.3 / 1.3
  d <- dst_mv_norm(c(a = 0, b = 0, c = 0), diag(3) + 0.3)
  # Equicorrelated trivariate orthant probability.
  expect_equal(
    eval_mv_cdf(d, list(0, 0, 0)),
    1 / 8 + 3 / (4 * pi) * asin(rho),
    tolerance = 1e-6
  )
  expect_identical(pretty_name(d), "Multivariate Normal")
})

test_that("the multivariate Normal draws with the right moments", {
  d <- dst_bi_norm(mean = c(a = 0, b = 1), sd = c(1, 2), cor = -0.4)
  set.seed(1)
  r <- realise(d, 1e5)
  expect_named(r, c("a", "b"))
  expect_equal(colMeans(r), c(a = 0, b = 1), tolerance = 0.02)
  expect_equal(stats::cor(r$a, r$b), -0.4, tolerance = 0.02)
})

test_that("the multivariate Normal checks its parameters", {
  expect_error(dst_mv_norm(c(0, 0), diag(3)), "one row and one column")
  lopsided <- matrix(c(1, 0.5, 0.2, 1), 2)
  expect_error(dst_mv_norm(c(0, 0), lopsided), "symmetric")
  expect_error(
    dst_mv_norm(c(0, 0), matrix(c(1, 2, 2, 1), 2)),
    "positive semi-definite"
  )
  expect_identical(vtype(dst_mv_norm(c(0, 0), matrix(1, 2, 2))), "singular")
  named <- diag(2)
  dimnames(named) <- list(c("a", "c"), c("a", "c"))
  expect_error(dst_mv_norm(c(a = 0, b = 0), named), "disagree")
  expect_error(dst_bi_norm(c(0, 0), c(1, 1), 1), "strictly between")
  expect_error(dst_bi_norm(c(0, 0), c(1, -1), 0), "positive")
  expect_true(is.na(dst_mv_norm(c(0, NA), diag(2))))
  expect_true(is.na(dst_bi_norm(c(0, 0), c(1, 1), NA)))
  # One variable is a univariate Normal.
  expect_identical(pretty_name(dst_mv_norm(3, matrix(4))), "Normal")
})

test_that("the empirical distribution keeps the observed points", {
  d <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(5, 4, 4, 6)))
  expect_identical(vtype(d), "discrete")
  expect_identical(nrow(support(d)[["points"]]), 3L)
  expect_equal(
    eval_mv_pmf(d, list(a = c(1, 2, 2), b = c(5, 4, 5))),
    c(0.25, 0.5, 0)
  )
  expect_equal(eval_mv_cdf(d, list(2, 5)), 0.75)
  expect_equal(eval_mv_survival(d, list(1, 4)), 0.25)
  expect_equal(mean(d), c(a = 2, b = 4.75))
  expect_equal(variance(d)["a", "a"], 0.5)
  set.seed(1)
  r <- realise(d, 10)
  expect_named(r, c("a", "b"))
  expect_true(all(paste(r$a, r$b) %in% c("1 5", "2 4", "3 6")))
})

test_that("empirical weights and NAs are handled", {
  l <- list(a = c(1, 2, NA), b = c(1, 2, 3))
  expect_true(is.na(dst_mv_empirical(l)))
  d <- dst_mv_empirical(l, na_action = "drop")
  expect_equal(eval_mv_pmf(d, list(1:2, 1:2)), c(0.5, 0.5))
  expect_error(dst_mv_empirical(l, na_action = "fail"), "NA")
  w <- dst_mv_empirical(list(1:2, 3:4), weights = c(3, 1))
  expect_equal(eval_mv_pmf(w, list(1:2, 3:4)), c(0.75, 0.25))
  zero <- dst_mv_empirical(list(1:2, 3:4), weights = c(1, 0))
  expect_identical(nrow(support(zero)[["points"]]), 1L)
  expect_error(dst_mv_empirical(list(1:2, 3:4), weights = c(1, -1)), "neg")
  expect_identical(pretty_name(dst_mv_empirical(list(1:3))), "Finite")
})

test_that("dst_bi_empirical() names variables after bare columns", {
  df <- data.frame(flow = c(10, 12, 15), depth = c(1, 1.5, 2), w = 1:3)
  d <- dst_bi_empirical(flow, depth, data = df)
  expect_identical(variables(d), c("flow", "depth"))
  w <- dst_bi_empirical(flow, depth, weights = w, data = df)
  expect_equal(eval_bi_pmf(w, 15, 2), 0.5)
  unnamed <- dst_bi_empirical(df$flow, df$depth)
  expect_identical(variables(unnamed), c("x", "y"))
  half <- dst_bi_empirical(flow, df$depth, data = df)
  expect_identical(variables(half), c("flow", "y"))
})

test_that("conditioning an empirical distribution uses its points", {
  d <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  expect_equal(eval_mv_cdf(d, list(2, 1:2), given = "a"), c(0.5, 1))
  expect_equal(eval_mv_pmf(d, list(2, 1), given = "a"), 0.5)
  expect_equal(eval_mv_survival(d, list(0, 1), given = "b"), 1)
  # Nothing observed at a = 5.
  expect_true(is.nan(eval_mv_cdf(d, list(5, 1), given = "a")))
})

test_that("printing names the variables", {
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  expect_output(print(d), "continuous; x, y")
  e <- dst_mv_empirical(list(a = 1:2, b = 3:4))
  expect_output(print(e), "prob")
  expect_output(print(support(e)), "2 points")
  expect_output(print(support(d)), "y: \\[-Inf, Inf\\]")
})
