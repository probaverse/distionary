test_that("orthant probabilities match their inclusion-exclusion", {
  skip_if_not_installed("mvtnorm")
  d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  cdf <- eval_bi_cdf(d, 0, 1)
  expect_equal(prob_bi_orthant(d, 0, 1, "<="), cdf)
  expect_equal(prob_bi_orthant(d, 0, 1, c("<=", ">")), 0.5 - cdf)
  expect_equal(prob_bi_orthant(d, 0, 1, c(">", "<=")), 0.5 - cdf)
  expect_equal(prob_bi_orthant(d, 0, 1, ">"), eval_bi_survival(d, 0, 1))
  # Strictness makes no difference to a continuous variable.
  expect_equal(prob_bi_orthant(d, 0, 1, c("<", ">=")), 0.5 - cdf)
  total <- prob_bi_orthant(d, 0, 1, c("<=", "<=")) +
    prob_bi_orthant(d, 0, 1, c("<=", ">")) +
    prob_bi_orthant(d, 0, 1, c(">", "<=")) +
    prob_bi_orthant(d, 0, 1, c(">", ">"))
  expect_equal(total, 1)
})

test_that("strict inequalities step past atoms", {
  h <- distribution(
    pmf = function(x, y) stats::dpois(x, 2) * stats::dbinom(y, 3, 0.5),
    cdf = function(x, y) stats::ppois(x, 2) * stats::pbinom(y, 3, 0.5),
    .support = support_product(n = discrete(natural0()), k = discrete(0:3))
  )
  expect_equal(
    prob_bi_orthant(h, 2, 1, c("<", ">=")),
    stats::ppois(1, 2) * stats::pbinom(0, 3, 0.5, lower.tail = FALSE)
  )
  expect_equal(
    prob_bi_orthant(h, 2, 1, c("<=", ">")),
    stats::ppois(2, 2) * stats::pbinom(1, 3, 0.5, lower.tail = FALSE)
  )
  # Below every atom.
  expect_equal(prob_bi_orthant(h, 0, 1, c("<", "<=")), 0)
})

test_that("`ineq` is checked and matched by name", {
  e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  expect_equal(
    prob_mv_orthant(e, list(a = 2, b = 1), ineq = c(b = "<=", a = "<")),
    0.25
  )
  expect_equal(prob_mv_orthant(e, list(2, 1), ineq = c("<=", "<=")), 0.5)
  expect_equal(prob_mv_orthant(e, list(2, 1), ineq = c(">=", ">")), 0.5)
  expect_error(prob_mv_orthant(e, list(2, 1), ineq = "=<"), "one of")
  expect_error(prob_mv_orthant(e, list(2, 1), ineq = rep("<", 3)), "one per")
})

test_that("a univariate orthant is prob_left() or prob_right()", {
  d <- dst_pois(3)
  expect_equal(
    prob_mv_orthant(d, list(2), "<"),
    prob_left(d, 2, inclusive = FALSE)
  )
  expect_equal(
    prob_mv_orthant(d, list(2), ">="),
    prob_right(d, 2, inclusive = TRUE)
  )
})

test_that("marginal() selects variables by name or position", {
  skip_if_not_installed("mvtnorm")
  d <- dst_mv_norm(
    mean = c(a = 0, b = 1, c = 2),
    cov = matrix(c(1, 0.2, 0.3, 0.2, 2, 0.4, 0.3, 0.4, 3), 3)
  )
  b <- marginal(d, "b")
  expect_identical(dimension(b), 1L)
  expect_equal(mean(b), 1)
  expect_equal(variance(b), 2)
  # A position among names is read as a name.
  expect_error(marginal(d, c("c", 1)), "does not have")
  ca <- marginal(d, c("c", "a"))
  expect_identical(variables(ca), c("c", "a"))
  expect_equal(unname(variance(ca)), matrix(c(3, 0.3, 0.3, 1), 2))
  expect_identical(marginal(d, 1:3), d)
  expect_error(marginal(d, character(0)), "at least one")
})

test_that("marginals are worked out when not stated", {
  d <- distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  )
  y <- marginal(d, "y")
  expect_identical(support(y), continuous(c(0, Inf)))
  expect_equal(eval_cdf(y, 1:2), stats::pexp(1:2))
  expect_equal(eval_survival(y, 1:2), stats::pexp(1:2, lower.tail = FALSE))
  expect_equal(eval_density(y, 1:2), stats::dexp(1:2))
  expect_equal(eval_quantile(y, 0.5), stats::qexp(0.5), tolerance = 1e-6)
  expect_equal(mean(y), 1, tolerance = 1e-6)
  expect_equal(mean(d), c(x = 0, y = 1), tolerance = 1e-6)
})

test_that("finite marginals add up the points", {
  e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  b <- marginal(e, "b")
  expect_equal(eval_pmf(b, 1:2), c(0.5, 0.5))
  a <- marginal(e, 1)
  expect_equal(eval_pmf(a, 1:3), c(0.25, 0.5, 0.25))
})
