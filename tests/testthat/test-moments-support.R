# Phase C: moments over a structured support (atom-sum + continuous quadrature).

# Remove the intrinsic moments so the numeric network algorithms are exercised.
strip_moments <- function(d) {
  for (m in c("mean", "variance", "stdev", "skewness", "kurtosis_exc",
              "kurtosis")) {
    d[[m]] <- NULL
  }
  d
}

# Compare network-computed moments against the family's analytic intrinsics.
expect_moments_recovered <- function(d, tol = 1e-4) {
  m0 <- mean(d)
  v0 <- variance(d)
  s0 <- skewness(d)
  k0 <- kurtosis_exc(d)
  d2 <- strip_moments(d)
  expect_equal(mean(d2), m0, tolerance = tol)
  expect_equal(variance(d2), v0, tolerance = tol)
  expect_equal(skewness(d2), s0, tolerance = tol)
  expect_equal(kurtosis_exc(d2), k0, tolerance = tol)
}

test_that("Moments of infinite discrete supports via walk-and-truncate.", {
  expect_moments_recovered(dst_pois(5))
  expect_moments_recovered(dst_pois(0.3))
  expect_moments_recovered(dst_geom(0.4))
  expect_moments_recovered(dst_nbinom(size = 6, prob = 0.45))
})

test_that("Moments of finite discrete supports by enumeration.", {
  expect_moments_recovered(dst_binom(20, 0.3))
  expect_moments_recovered(dst_hyper(m = 7, n = 5, k = 6))
  expect_moments_recovered(dst_finite(c(-2, 0, 3, 8), probs = c(.1, .4, .3, .2)))
})

test_that("Continuous moments still computed via quadrature (regression).", {
  expect_moments_recovered(dst_norm(2, 3))
  expect_moments_recovered(dst_exp(1.5))
  expect_moments_recovered(dst_beta(2, 5))
})

test_that("Mixed-distribution moments combine atom-sum and quadrature.", {
  # Atom at 0 with mass 0.3, plus a continuous Uniform(0, 1) part with mass 0.7.
  d <- distribution(
    pmf = function(x) ifelse(x == 0, 0.3, 0),
    density = function(x) ifelse(x > 0 & x <= 1, 0.7, 0),
    cdf = function(x) 0.3 * (x >= 0) + 0.7 * pmin(pmax(x, 0), 1),
    .support = mixed(atoms = 0, continuous = c(0, 1))
  )
  expect_equal(vtype(d), "mixed")
  # E[X] = 0 * 0.3 + integral_0^1 x * 0.7 dx = 0.35
  expect_equal(mean(d), 0.35, tolerance = 1e-6)
  # Var = E[(X - mu)^2] = (0 - .35)^2 * .3 + 0.7 * integral_0^1 (x - .35)^2 dx
  expect_equal(variance(d), 0.1108333, tolerance = 1e-6)
})

test_that("A union-of-intervals continuous part integrates over each piece.", {
  # Uniform on [0, 1] U [2, 3]: density 1/2 on each, mean 1.5 by symmetry.
  d <- distribution(
    density = function(x) ifelse((x >= 0 & x <= 1) | (x >= 2 & x <= 3), 0.5, 0),
    cdf = function(x) {
      0.5 * pmin(pmax(x, 0), 1) + 0.5 * pmin(pmax(x - 2, 0), 1)
    },
    .support = continuous(c(0, 1), c(2, 3))
  )
  expect_equal(mean(d), 1.5, tolerance = 1e-6)
})

test_that("Legacy continuous distributions (no support) still compute moments.", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  d <- distribution(
    density = function(x) stats::dnorm(x),
    cdf = function(x) stats::pnorm(x),
    .vtype = "continuous"
  )
  expect_equal(mean(d), 0, tolerance = 1e-6)
  expect_equal(variance(d), 1, tolerance = 1e-6)
})
