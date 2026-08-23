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
    .support = mixed(discrete = 0, continuous = c(0, 1))
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

test_that("Moments integrate over the support that was declared.", {
  d <- distribution(
    density = function(x) stats::dnorm(x),
    cdf = function(x) stats::pnorm(x),
    .support = continuous(c(-Inf, Inf))
  )
  expect_equal(mean(d), 0, tolerance = 1e-6)
  expect_equal(variance(d), 1, tolerance = 1e-6)
})

test_that("Numerical moments require a support.", {
  # Unreachable through `distribution()`, which insists on one; checked
  # directly so the guard does not rot.
  bare <- distionary:::new_distribution(
    list(density = stats::dnorm, cdf = stats::pnorm),
    vtype = "continuous", name = "Bare"
  )
  expect_error(
    distionary:::expect_over_support(bare, function(x) x),
    "requires the distribution's support"
  )
})

test_that("Atoms accumulating at an interior sink from both sides are summed.", {
  # Atoms 5 - 2^(-n) and 5 + 2^(-n), n = 1, 2, ...: sinks at 5 from both
  # sides. A naive outward walk would stall at 5 and miss the far side.
  below <- discretes::dsct_transform(
    natural1(),
    fun = function(n) 5 - 2^(-n), inv = function(y) -log2(5 - y),
    domain = c(0, Inf), range = c(4, 5), dir = "increasing"
  )
  above <- discretes::dsct_transform(
    natural1(),
    fun = function(n) 5 + 2^(-n), inv = function(y) -log2(y - 5),
    domain = c(0, Inf), range = c(5, 6), dir = "decreasing"
  )
  both <- discretes::dsct_union(below, above)
  # Mass 0.5 * 2^(-n) on each side's n-th atom; sums to 1 overall.
  pmf <- function(x) {
    n <- ifelse(x < 5, -log2(5 - x), -log2(x - 5))
    0.5 * 2^(-round(n))
  }
  # The walk also needs to know how much probability is still ahead of it,
  # which is what the cdf says. Below 5, the atoms passed are the first N,
  # where N = floor(-log2(5 - x)). Above 5, every atom below has been passed,
  # plus the above-side atoms from M = ceiling(-log2(x - 5)) onward. At 5
  # itself only the below side counts: 5 is where the atoms pile up, not one
  # of them.
  cdfun <- function(x) {
    vapply(x, function(z) {
      if (z < 4.5) {
        return(0)
      }
      if (z < 5) {
        n <- floor(-log2(5 - z))
        return(0.5 * (1 - 2^(-n)))
      }
      if (z == 5) {
        return(0.5)
      }
      m <- ceiling(-log2(z - 5))
      0.5 + if (m > 0) 2^(-m) else 0.5
    }, numeric(1))
  }
  # Check that cdf against the masses it is meant to be accumulating, by
  # adding up the pmf over the atoms that actually lie at or below a point.
  # Comparing it against its own formula would prove nothing.
  atoms_below <- 5 - 2^(-(1:40))
  atoms_above <- 5 + 2^(-(1:40))
  by_summing <- function(z) {
    sum(pmf(atoms_below[atoms_below <= z])) +
      sum(pmf(atoms_above[atoms_above <= z]))
  }
  for (z in c(4.6, 4.9, 4.99, 5, 5.2, 5.5, 6)) {
    expect_equal(cdfun(z), by_summing(z), tolerance = 1e-9)
  }

  total <- sum_over_atoms(both, pmf, cdfun, function(x) rep(1, length(x)))
  expect_equal(total, 1, tolerance = 1e-6)
  mu <- sum_over_atoms(both, pmf, cdfun, function(x) x)
  expect_equal(mu, 5, tolerance = 1e-6) # Symmetric about the sink.
  # E[(X - 5)^2] = sum_n 2^(-n) * 4^(-n) = sum_n 8^(-n) = 1/7.
  v <- sum_over_atoms(both, pmf, cdfun, function(x) (x - 5)^2)
  expect_equal(v, 1 / 7, tolerance = 1e-6)
})

test_that("A divergent numerical moment gives NaN.", {
  # Continuous, both tails diverge (the Cauchy mean).
  expect_true(
    is.nan(suppressMessages(eval_mean_from_network(dst_cauchy(0, 1))))
  )
  # Continuous, a single tail diverges (a half-Cauchy on [0, Inf)).
  half_cauchy <- distribution(
    density = function(x) ifelse(x >= 0, 2 * stats::dcauchy(x), 0),
    cdf = function(x) ifelse(x < 0, 0, 2 * (stats::pcauchy(x) - 0.5)),
    .support = continuous(c(0, Inf))
  )
  expect_true(is.nan(suppressMessages(mean(half_cauchy))))
  # Discrete heavy tail: pmf 1/(k (k + 1)) on k = 1, 2, ... sums to 1
  # (telescoping), but the mean sum_k 1/(k + 1) does not converge.
  heavy <- distribution(
    pmf = function(x) ifelse(x >= 1 & x == round(x), 1 / (x * (x + 1)), 0),
    cdf = function(x) ifelse(x < 1, 0, 1 - 1 / (floor(x) + 1)),
    .support = discrete(natural1())
  )
  expect_true(is.nan(suppressMessages(mean(heavy))))
})

test_that("The walk does not stop while probability is still ahead.", {
  # 0.8 of the probability at zero, then a long stretch of atoms each below
  # tolerance in both mass and contribution, then 0.2 beyond the stretch.
  # Judging by the atoms underfoot alone, the walk goes quiet in the gap and
  # never reaches the mass past it.
  tiny <- 1e-12
  far <- 201
  pm <- function(x) {
    out <- numeric(length(x))
    out[x == 0] <- 0.8
    out[x >= 1 & x <= 200] <- tiny
    out[x == far] <- 0.2 - 200 * tiny
    out
  }
  cdfun <- function(x) {
    vapply(x, function(z) {
      if (z < 0) {
        return(0)
      }
      sum(pm(0:max(0, floor(z))))
    }, numeric(1))
  }
  d <- distribution(pmf = pm, cdf = cdfun, .support = discrete(natural0()))
  expect_equal(mean(d), sum((0:far) * pm(0:far)), tolerance = 1e-6)
  expect_equal(mean(d), 40.2, tolerance = 1e-6)
})
