
test_that("Object is a distribution.", {
  expect_true(is_distribution(
    distribution(cdf = pnorm, density = dnorm, .support = continuous())
  ))
})

test_that("distribution() requires a support.", {
  expect_error(distribution(cdf = pnorm, density = dnorm), "needs a support")
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

test_that("`.vtype` is defunct.", {
  # It errors whether or not a support is given, and the message names the
  # replacement -- which is the whole reason the argument is still there.
  lifecycle::expect_defunct(
    distribution(cdf = pnorm, density = dnorm, .vtype = "continuous")
  )
  lifecycle::expect_defunct(
    distribution(
      cdf = pnorm, density = dnorm,
      .support = continuous(), .vtype = "continuous"
    )
  )
  # Checked before the support, so old code gets the useful message rather
  # than the generic one.
  expect_error(
    distribution(cdf = pnorm, density = dnorm, .vtype = "continuous"),
    "Please use the `.support` argument"
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
