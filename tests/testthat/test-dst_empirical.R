y <- c(5, 2, 4, 2, NA, 2, -3, 8, 9)
w <- c(2, 2, 0, 2, 10, 1, NA, -1, 1)
dat <- data.frame(y = y, w = w)

edist <- dst_empirical(y, data = dat)
cdf <- representation_as_function(edist, "cdf")
qf <- representation_as_function(edist, "quantile")
sf <- representation_as_function(edist, "survival")

set.seed(5)
edist2 <- dst_empirical(rnorm(10))

test_that("unweighted empirical distribution works", {
  cdf2 <- representation_as_function(edist2, "cdf")
  expect_true(is_empirical(edist))
  expect_true(is_empirical(edist2))
  expect_true(is_distribution(edist))
  expect_true(is_distribution(edist2))
  expect_equal(cdf(qf(1 / 8)), 1 / 8)
  expect_equal(cdf(qf(1 / 8 + 0.001)), 4 / 8)
  expect_equal(cdf(qf(1 / 8 - 0.001)), 1 / 8)
  expect_equal(qf(4:5 / 8), c(2, 4))
  expect_equal(qf(cdf(2)), 2)
  expect_equal(qf(cdf(2 + 0.001)), 2)
  expect_equal(qf(cdf(2 - 0.001)), -3)
  expect_equal(qf(0:1), c(-3, 9))
  set.seed(1)
  s1 <- sample(sort(unique(y)), size = 100, replace = TRUE,
               prob = c(1, 3, 1, 1, 1, 1))
  set.seed(1)
  s2 <- realise(edist, 100)
  expect_equal(s1, s2)
  set.seed(1)
  at <- c(y, rnorm(10))
  expect_identical(eval_survival(edist, at), 1 - cdf(at))
})
