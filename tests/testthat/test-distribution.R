
test_that("Object is a distribution.", {
  expect_true(is_distribution(
    distribution(cdf = pnorm, density = dnorm, .support = continuous())
  ))
})

test_that("distribution() requires a support.", {
  expect_error(distribution(cdf = pnorm, density = dnorm), "needs a support")
  # A variable type is not a substitute: it says what kind of probability
  # there is, not where it lives.
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    distribution(cdf = pnorm, density = dnorm, .vtype = "continuous"),
    "needs a support"
  )
  # An empty support is a support, but not one a distribution can have.
  expect_error(
    distribution(cdf = pnorm, density = dnorm, .support = empty_support()),
    "cannot have an empty support"
  )
})

test_that("distribution() edge cases satisfied.", {
  s <- continuous()
  suppressWarnings({
    expect_error(distribution(1:10, .support = s))
    expect_error(distribution(.parameters = "foofy", .support = s))
    expect_error(distribution(.parameters = list("foofy"), .support = s))
    expect_error(distribution(.parameters = c(alpha = 4), .support = s))
    expect_error(distribution(.name = c("my", "name", "is"), .support = s))
    expect_error(distribution(.name = character(0), .support = s))
  })
})

test_that("`.vtype` is superseded, and now has no effect.", {
  # The variable type is derived from the support, so whatever `.vtype` says
  # -- including nonsense -- is ignored rather than corrected or complained
  # about. It warns only that the argument itself is on its way out.
  rlang::local_options(lifecycle_verbosity = "quiet")
  d <- distribution(
    cdf = pnorm, density = dnorm,
    .support = discrete(1:3),
    .vtype = "continuous"
  )
  expect_equal(vtype(d), "discrete")
  expect_no_error(
    distribution(
      cdf = pnorm, density = dnorm,
      .support = continuous(), .vtype = "contnuous"
    )
  )
})

test_that("`.vtype` still refuses a support object.", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    distribution(
      cdf = pnorm, density = dnorm,
      .support = continuous(), .vtype = continuous()
    ),
    "accepts only a character"
  )
})

test_that("Warning when cdf and pmf/density missing.", {
  s <- continuous()
  expect_warning(
    distribution(.support = s)
  )
  expect_warning(
    distribution(pmf = dpois, quantile = qpois, .support = s)
  )
  expect_warning(
    distribution(density = dnorm, quantile = qnorm, .support = s)
  )
  expect_no_warning(
    distribution(cdf = pnorm, density = dnorm, .support = s)
  )
  expect_no_warning(
    distribution(cdf = pnorm, pmf = dpois, .support = s)
  )
})
