# An independent Normal x Exponential, stating only a density and a CDF, so
# that everything else comes from the network.
indep <- function() {
  distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  )
}

test_that("bivariate and multivariate evaluators agree", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  x <- c(-1, 0, 2)
  y <- c(0, 1, 3)
  expect_equal(eval_bi_cdf(d, x, y), eval_mv_cdf(d, list(x, y)))
  expect_equal(eval_bi_survival(d, x, y), eval_mv_survival(d, list(x, y)))
  expect_equal(eval_bi_density(d, x, y), eval_mv_density(d, list(x, y)))
  expect_equal(eval_bi_pmf(d, x, y), c(0, 0, 0))
})

test_that("`l` is matched by name, recycled, and may be a data frame", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(a = 0, b = 1), sd = c(1, 2), cor = 0.6)
  expect_equal(
    eval_mv_cdf(d, list(b = 1, a = 0)),
    eval_mv_cdf(d, list(0, 1))
  )
  expect_equal(
    eval_mv_density(d, list(a = 0, b = 1:3)),
    eval_mv_density(d, list(c(0, 0, 0), 1:3))
  )
  df <- data.frame(b = 1:2, a = c(0, 0.5))
  expect_equal(
    eval_mv_density(d, df),
    eval_mv_density(d, list(c(0, 0.5), 1:2))
  )
  expect_error(eval_mv_cdf(d, list(a = 0, z = 1)), "does not have")
  expect_error(eval_mv_cdf(d, list(a = 0, 1)), "some vectors")
  expect_error(eval_mv_cdf(d, list(0)), "one vector per variable")
  expect_error(eval_mv_cdf(d, c(0, 1)), "list of vectors")
  expect_error(eval_mv_cdf(d, list(1:2, 1:3)))
})

test_that("the multivariate evaluators accept a univariate distribution", {
  d <- dst_norm(0, 1)
  expect_equal(eval_mv_cdf(d, list(-1:1)), stats::pnorm(-1:1))
  expect_equal(eval_mv_density(d, list(x = 0)), stats::dnorm(0))
})

test_that("functions for one variable refuse several, and say what to use", {
  d <- indep()
  expect_error(eval_cdf(d, 0), "eval_bi_cdf")
  expect_error(eval_density(d, 0), "eval_mv_density")
  expect_error(eval_quantile(d, 0.5), "marginal")
  expect_error(range(d), "marginal")
  expect_error(prob_left(d, 0, inclusive = TRUE), "marginal")
  expect_error(median(d), "marginal")
  expect_error(eval_bi_cdf(dst_norm(0, 1), 0, 0), "two variables")
})

test_that("survival is every variable exceeding, not one minus the CDF", {
  d <- indep()
  expect_equal(eval_bi_survival(d, 0, 1), 0.5 * exp(-1))
  expect_false(isTRUE(all.equal(
    eval_bi_survival(d, 0, 1),
    1 - eval_bi_cdf(d, 0, 1)
  )))
})

test_that("the CDF is worked out from a stated survival function", {
  # distribution() warns without a `cdf`; a survival function is enough.
  d <- suppressWarnings(distribution(
    survival = function(x, y) {
      stats::pnorm(x, lower.tail = FALSE) * stats::pexp(y, lower.tail = FALSE)
    },
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  ))
  expect_equal(
    eval_bi_cdf(d, c(-1, 0.5), c(0.2, 3)),
    stats::pnorm(c(-1, 0.5)) * stats::pexp(c(0.2, 3))
  )
})

test_that("`given` conditions on variables by name, position, or argument", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  # Y | X = x is Normal with mean 1 + 0.6 * 2 * x, sd 2 * sqrt(1 - 0.36).
  truth <- stats::pnorm(0:2, mean = 1 + 1.2, sd = 1.6)
  expect_equal(eval_bi_cdf(d, x = 1, y = 0:2, given = "x"), truth)
  expect_equal(eval_bi_cdf(d, x = 1, y = 0:2, given = 1), truth)
  named <- dst_bi_norm(mean = c(u = 0, v = 1), sd = c(1, 2), cor = 0.6)
  expect_equal(eval_bi_cdf(named, x = 1, y = 0:2, given = "u"), truth)
  # "x" is also the argument, when no variable is called that.
  expect_equal(eval_bi_cdf(named, x = 1, y = 0:2, given = "x"), truth)
  expect_equal(
    eval_bi_survival(d, x = 1, y = 0:2, given = "x"),
    1 - truth
  )
  expect_equal(
    eval_bi_density(d, x = 1, y = 0:2, given = "x"),
    stats::dnorm(0:2, mean = 2.2, sd = 1.6)
  )
  # X | Y = y is Normal with mean 0.6 / 2 * (y - 1), sd sqrt(1 - 0.36).
  expect_equal(
    eval_bi_cdf(d, x = 0.5, y = 3, given = "y"),
    stats::pnorm(0.5, mean = 0.6, sd = 0.8)
  )
  expect_error(eval_bi_cdf(d, 0, 0, given = c("x", "y")), "nothing to")
  expect_error(eval_bi_cdf(d, 0, 0, given = "z"), "does not have")
  expect_error(eval_bi_cdf(d, 0, 0, given = 3), "position")
})

test_that("variable names take precedence over the argument names", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(y = 0, x = 10), sd = c(1, 1), cor = 0.5)
  # `given = "x"` is the variable named x: the second one.
  expect_equal(
    eval_bi_cdf(d, x = 0, y = 10, given = "x"),
    eval_bi_cdf(d, x = 0, y = 10, given = 2)
  )
})

test_that("conditionals are worked out by integration when not stated", {
  d <- indep()
  expect_equal(eval_bi_cdf(d, 0.3, 1:2, given = "x"), stats::pexp(1:2))
  expect_equal(
    eval_bi_survival(d, 0.3, 1, given = "y"),
    stats::pnorm(0.3, lower.tail = FALSE)
  )
  expect_equal(eval_bi_density(d, 0.3, 1, given = "x"), stats::dexp(1))
})

test_that("NA propagates", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  expect_identical(is.na(eval_bi_cdf(d, c(0, NA), 1)), c(FALSE, TRUE))
  expect_identical(is.na(eval_bi_density(d, 0, c(NA, 1))), c(TRUE, FALSE))
  expect_identical(
    is.na(eval_bi_cdf(d, c(NA, 0), 1, given = "x")),
    c(TRUE, FALSE)
  )
})
