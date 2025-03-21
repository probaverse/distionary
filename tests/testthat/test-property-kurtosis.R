test_that("Network is invoked in priority: stdev", {
  d <- distribution(
    density = stats::dnorm,
    range = c(-Inf, Inf),
    kurtosis = 100, # deliberately incorrect
    .vtype = "continuous"
  )
  # Standard deviation should come from built-in variance.
  expect_equal(kurtosis_exc(d), 100 - 3)
  expect_equal(eval_kurtosis_exc_from_network(d), 100 - 3)
  # Remove variance, and sd of 3 should be retrieved through density.
  d$kurtosis <- NULL
  expect_equal(kurtosis_exc(d), 0)
  expect_equal(eval_kurtosis_exc_from_network(d), 0)
})

test_that("Network is invoked in priority: variance", {
  d <- distribution(
    density = stats::dnorm,
    range = c(-Inf, Inf),
    kurtosis_exc = 100 - 3, # deliberately incorrect
    .vtype = "continuous"
  )
  # Standard deviation should come from built-in variance.
  expect_equal(kurtosis(d), 100)
  expect_equal(eval_kurtosis_from_network(d), 100)
  # Remove variance, and sd of 3 should be retrieved through density.
  d$kurtosis_exc <- NULL
  expect_equal(kurtosis(d), 3)
  expect_equal(eval_kurtosis_from_network(d), 3)
})

test_that("Mean calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "mean")) {
        expect_equal(eval_mean_from_network(d), mean(d))
      }
    }
  }
})

test_that("Kurtosis calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "kurtosis")) {
        expect_equal(eval_kurtosis_from_network(d), kurtosis(d))
        if (is_intrinsic(d, "kurtosis_exc")) {
          d[["kurtosis_exc"]] <- NULL
          expect_equal(eval_kurtosis_from_network(d), kurtosis(d))
        }
      }
    }
  }
})

test_that("Excess kurtosis calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "kurtosis_exc")) {
        expect_equal(eval_kurtosis_exc_from_network(d), kurtosis_exc(d))
        if (is_intrinsic(d, "kurtosis")) {
          d[["kurtosis"]] <- NULL
          expect_equal(eval_kurtosis_exc_from_network(d), kurtosis_exc(d))
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
#   x <- seq(r[1], r[2], by = 1L)
#   x4 <- ((x - mu) / sigma)^4
#   p <- eval_pmf(distribution, at = x)
#   return(sum(p * x4))
# }
# if (attr(distribution, "name") %in% c(
#   "Negative Binomial", "Poisson", "Geometric"
# )) {
#   to_add <- Inf
#   i <- 0
#   kur <- 0
#   while (to_add > 1e-9) {
#     x <- 0:99 + 100 * i
#     to_add <- sum(eval_pmf(distribution, x) * ((x - mu) / sigma)^4)
#     kur <- kur + to_add
#     i <- i + 1
#   }
#   return(kur)
# }
