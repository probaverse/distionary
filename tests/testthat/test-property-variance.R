test_that("Network is invoked in priority: stdev", {
  d <- distribution(
    density = \(x) stats::dnorm(x, sd = 3),
    range = c(-Inf, Inf),
    variance = 100, # deliberately incorrect
    .vtype = "continuous"
  )
  # Standard deviation should come from built-in variance.
  expect_equal(stdev(d), 10)
  expect_equal(eval_stdev_from_network(d), 10)
  # Remove variance, and sd of 3 should be retrieved through density.
  d$variance <- NULL
  expect_equal(stdev(d), 3)
  expect_equal(eval_stdev_from_network(d), 3)
})

test_that("Network is invoked in priority: variance", {
  d <- distribution(
    density = \(x) stats::dnorm(x, sd = 3),
    range = c(-Inf, Inf),
    stdev = 10, # deliberately incorrect
    .vtype = "continuous"
  )
  # Standard deviation should come from built-in variance.
  expect_equal(variance(d), 100)
  expect_equal(eval_variance_from_network(d), 100)
  # Remove variance, and sd of 3 should be retrieved through density.
  d$stdev <- NULL
  expect_equal(variance(d), 9)
  expect_equal(eval_variance_from_network(d), 9)
})

test_that("Variance calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "variance")) {
        expect_equal(eval_variance_from_network(d), variance(d))
        if (is_intrinsic(d, "stdev")) {
          d[["stdev"]] <- NULL
          expect_equal(eval_variance_from_network(d), variance(d))
        }
      }
    }
  }
})

test_that("Standard dev calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "stdev")) {
        expect_equal(eval_stdev_from_network(d), stdev(d))
        if (is_intrinsic(d, "variance")) {
          d[["variance"]] <- NULL
          expect_equal(eval_stdev_from_network(d), stdev(d))
        }
      }
    }
  }
})

# if (attr(distribution, "name") %in% c(
#   "Hypergeometric", "Bernoulli", "Binomial"
# )) {
#   # This case is for double-checking the moments supplied for these
#   # distributions, and will be included until discretes handling is
#   # implemented.
#   r <- range(distribution)
#   x <- seq(r[1], r[2], by = 1L)
#   x2 <- (x - mu)^2
#   p <- eval_pmf(distribution, at = x)
#   return(sum(p * x2))
# }
# if (attr(distribution, "name") %in% c(
#   "Negative Binomial", "Poisson", "Geometric"
# )) {
#   to_add <- Inf
#   i <- 0
#   variance <- 0
#   while (to_add > 1e-9) {
#     x <- 0:99 + 100 * i
#     to_add <- sum(eval_pmf(distribution, x) * (x - mu)^2)
#     variance <- variance + to_add
#     i <- i + 1
#   }
#   return(variance)
# }
