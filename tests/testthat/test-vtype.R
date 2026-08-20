test_that("Variable type specified correctly.", {
  expect_equal(vtype(dst_bern(0.5)), "discrete")
  expect_equal(vtype(dst_beta(1, 1)), "continuous")
  expect_equal(vtype(dst_binom(size = 1, prob = 0.4)), "discrete")
  expect_equal(vtype(dst_cauchy(1, 1)), "continuous")
  expect_equal(vtype(dst_chisq(1)), "continuous")
  expect_equal(vtype(dst_degenerate(1)), "discrete")
  expect_equal(vtype(dst_exp(1)), "continuous")
  expect_equal(vtype(dst_f(1, 1)), "continuous")
  expect_equal(vtype(dst_gamma(1, 1)), "continuous")
  expect_equal(vtype(dst_geom(0.5)), "discrete")
  expect_equal(vtype(dst_gev(1, 1, 1)), "continuous")
  expect_equal(vtype(dst_gp(1, 1)), "continuous")
  expect_equal(vtype(dst_hyper(1, 1, 1)), "discrete")
  expect_equal(vtype(dst_lnorm(1, 1)), "continuous")
  expect_equal(vtype(dst_lp3(1, 1, 1)), "continuous")
  expect_equal(vtype(dst_nbinom(size = 1, prob = 0.4)), "discrete")
  expect_equal(vtype(dst_norm(1, 1)), "continuous")
  expect_true(is.na(vtype(dst_null())))
  expect_equal(vtype(dst_pearson3(1, 1, 1)), "continuous")
  expect_equal(vtype(dst_pois(1)), "discrete")
  expect_equal(vtype(dst_t(1)), "continuous")
  expect_equal(vtype(dst_unif(0, 1)), "continuous")
  expect_equal(vtype(dst_weibull(1, 1)), "continuous")
})

test_that("Variable type is derived from the support that was given", {
  f <- function(x) x
  suppressWarnings({
    expect_identical(
      vtype(distribution(cdf = f, density = f, .support = continuous())),
      "continuous"
    )
    expect_identical(
      vtype(distribution(cdf = f, pmf = f, .support = discrete(1:3))),
      "discrete"
    )
    expect_identical(
      vtype(distribution(
        cdf = f, density = f,
        .support = mixed(atoms = 0, continuous = c(0, 1))
      )),
      "mixed"
    )
  })
  # There is no other way to set it: `.vtype` is defunct.
  lifecycle::expect_defunct(
    distribution(cdf = f, density = f, .support = discrete(1:3),
                 .vtype = "foofy")
  )
})

test_that("The Null distribution has no variable type", {
  expect_identical(vtype(dst_null()), NA_character_)
})
