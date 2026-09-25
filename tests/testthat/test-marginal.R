test_that("marginal() selects variables by name or position", {
  skip_if_not_installed("mvtnorm")
  d <- dst_mv_norm(
    mean = c(a = 0, b = 1, c = 2),
    cov = matrix(c(1, 0.2, 0.3, 0.2, 2, 0.4, 0.3, 0.4, 3), 3)
  )
  b <- marginal(d, "b")
  expect_identical(dimension(b), 1L)
  expect_identical(variables(b), "b")
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

test_that("selecting every variable in a new order reorders exactly", {
  g <- distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(
      rainfall = continuous(),
      runoff = continuous(c(0, Inf))
    )
  )
  r <- marginal(g, c("runoff", "rainfall"))
  expect_identical(variables(r), c("runoff", "rainfall"))
  expect_equal(eval_bi_density(r, 1, 0.3), eval_bi_density(g, 0.3, 1))
  expect_equal(eval_bi_cdf(r, 1, 0.3), eval_bi_cdf(g, 0.3, 1))
  expect_identical(support_marginal(support(r), 1L), continuous(c(0, Inf)))
  # Stated properties are kept, with positions translated.
  u <- permute_distribution(dst_mv_norm(c(p = 0, q = 5), diag(2)), 2:1)
  expect_identical(variables(u), c("q", "p"))
  expect_equal(mean(u), c(q = 5, p = 0))
  expect_equal(prob(u, q - p > 5), 0.5)
  expect_equal(prob(u, q > 5, given = p == 1), 0.5)
  e <- dst_mv_empirical(list(a = c(1, 2, 2), b = c(3, 4, 4)))
  expect_equal(eval_mv_pmf(marginal(e, c("b", "a")), list(4, 2)), 2 / 3)
})

test_that("a reordering cannot separate paired variables", {
  sp <- support_product(
    discrete(data.frame(a = 1:2, b = 3:4)),
    z = continuous()
  )
  h <- suppressWarnings(distribution(cdf = function(a, b, z) 0, .support = sp))
  expect_error(marginal(h, c("a", "z", "b")), "paired")
  expect_identical(variables(marginal(h, c("z", "a", "b"))), c("z", "a", "b"))
})
