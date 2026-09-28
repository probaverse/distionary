# Two rivers and their total, as a singular trivariate Normal.
rivers <- function() {
  mu <- c(r1 = 50, r2 = 80)
  sigma <- matrix(c(100, 60, 60, 225), 2)
  a <- rbind(r1 = c(1, 0), r2 = c(0, 1), s = c(1, 1))
  dst_mv_norm(
    mean = stats::setNames(as.numeric(a %*% mu), rownames(a)),
    cov = a %*% sigma %*% t(a)
  )
}

test_that("a linear combination makes a singular Normal", {
  skip_if_not_installed("mvtnorm")
  trio <- rivers()
  expect_identical(vtype(trio), "singular")
  expect_s3_class(support(trio), "support_affine")
  expect_error(eval_mv_density(trio, list(1, 1, 2)), "no density")
  expect_identical(eval_mv_pmf(trio, list(1, 1, 2)), 0)
  # P(r1 <= 50, r2 <= 80, s <= 130) is P(r1 <= 50, r2 <= 80).
  full <- dst_mv_norm(c(r1 = 50, r2 = 80), matrix(c(100, 60, 60, 225), 2))
  expect_equal(
    eval_mv_cdf(trio, list(50, 80, 130)),
    eval_bi_cdf(full, 50, 80),
    tolerance = 1e-5
  )
  set.seed(1)
  draws <- realise(trio, 5)
  expect_equal(draws$s, draws$r1 + draws$r2)
})

test_that("conditioning on the total slices the distribution", {
  skip_if_not_installed("mvtnorm")
  sl <- condition(rivers(), given = c(s = 200))
  expect_identical(variables(sl), c("r1", "r2"))
  expect_identical(vtype(sl), "singular")
  expect_equal(unname(rowSums(realise(sl, 4))), rep(200, 4))
  # r1 given s: mean 50 + 160 / 445 * 70, variance 100 - 160^2 / 445.
  m <- 50 + 160 / 445 * 70
  sd <- sqrt(100 - 160^2 / 445)
  r1 <- marginal(sl, "r1")
  expect_equal(mean(r1), m)
  expect_equal(stdev(r1), sd)
  # P(r1 <= 60, r2 <= 150) on the line is P(50 <= r1 <= 60).
  expect_equal(
    eval_bi_cdf(sl, 60, 150),
    diff(stats::pnorm(c(50, 60), m, sd)),
    tolerance = 1e-6
  )
  expect_equal(
    eval_mv_cdf(rivers(), list(60, 150, 200), known = "s"),
    eval_bi_cdf(sl, 60, 150)
  )
})

test_that("the slice does not disturb the random number stream", {
  skip_if_not_installed("mvtnorm")
  sl <- condition(rivers(), given = c(s = 200))
  set.seed(5)
  a <- stats::runif(1)
  eval_bi_cdf(sl, 60, 150)
  b <- stats::runif(1)
  set.seed(5)
  expect_identical(c(a, b), stats::runif(2))
})

test_that("impossible conditioning gives the Null distribution", {
  trio <- rivers()
  # x and y are always equal, so x = 1 and y = 2 cannot happen together.
  tied <- dst_mv_norm(
    c(x = 0, y = 0, z = 0),
    matrix(c(1, 1, 0, 1, 1, 0, 0, 0, 1), 3)
  )
  expect_true(is.na(condition(tied, c(x = 1, y = 2))))
  expect_equal(mean(condition(tied, c(x = 1, y = 1))), 0)
  expect_identical(
    eval_quantile(condition(trio, c(r1 = 40, r2 = 10)), 0.5),
    50
  )
})

test_that("conditionals are worked out when not stated", {
  g <- distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  )
  y <- condition(g, c(x = 0.3))
  expect_equal(eval_density(y, 1:2), stats::dexp(1:2))
  expect_equal(eval_cdf(y, 1:2), stats::pexp(1:2))
  expect_equal(mean(y), 1, tolerance = 1e-6)
  e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  b <- condition(e, list(a = 2))
  expect_equal(eval_pmf(b, 1:2), c(0.5, 0.5))
  expect_true(is.na(condition(e, c(a = 9))))
})
