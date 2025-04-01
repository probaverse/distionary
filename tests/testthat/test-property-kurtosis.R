test_that("Network is invoked in priority: kurtosis", {
  # First look for `kurtosis_exc`, then invoke algorithm if not found.
  d <- distribution(
    density = stats::dnorm,
    range = c(-Inf, Inf),
    kurtosis_exc = 100 - 3, # deliberately incorrect
    .vtype = "continuous"
  )
  expect_equal(kurtosis(d), 100)
  expect_equal(eval_kurtosis_from_network(d), 100)
  d$kurtosis_exc <- NULL
  expect_equal(kurtosis(d), 3)
  expect_equal(eval_kurtosis_from_network(d), 3)
})

test_that("Original kurtosis is never specified in our distributions", {
  # ...so don't need to verify their entries.
  for (item in test_distributions) {
    d <- rlang::exec(item$distribution, !!!item$valid[[1]])
    expect_false(is_intrinsic(d, "kurtosis"))
  }
})

test_that("Kurtosis algorithm matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "kurtosis_exc") || is_intrinsic(d, "kurtosis")) {
        supposed_kurt <- kurtosis(d)
        if (is.infinite(supposed_kurt)) {
          supposed_kurt <- NaN  # To align with numerical integration output.
        }
        if (vtype(d) == "continuous") {
          expect_equal(suppressMessages(algorithm_kurtosis(d)), supposed_kurt)
        } else if (
          pretty_name(d) %in%
          c("Hypergeometric", "Bernoulli", "Binomial")
        ) {
          expect_error(algorithm_kurtosis(d))
          r <- range(d)
          x <- seq(r[1], r[2], by = 1L)
          mu <- mean(d)
          sigma <- stdev(d)
          x4 <- ((x - mu) / sigma)^4
          p <- eval_pmf(d, at = x)
          expect_equal(sum(p * x4), supposed_kurt)
        } else if (
          pretty_name(d) %in%
          c("Negative Binomial", "Poisson", "Geometric")
        ) {
          expect_error(algorithm_kurtosis(d))
          mu <- mean(d)
          sigma <- stdev(d)
          to_add <- Inf
          i <- 0
          kur <- 0
          while (to_add > 1e-9) {
            x <- 0:99 + 100 * i
            to_add <- sum(eval_pmf(d, x) * ((x - mu) / sigma)^4)
            kur <- kur + to_add
            i <- i + 1
          }
          expect_equal(kur, supposed_kurt)
        } else if (pretty_name(d) == "Degenerate") {
          # Do nothing; checked in a separate script.
        } else {
          # Shouldn't be any distributions left.
          stop("At least one distribution family has not been accounted for.")
        }
      }
    }
  }
})

test_that("Kurtosis algorithm takes longer for smaller tolerance.", {
  d <- dst_norm(0, 1)
  t1 <- system.time(suppressMessages(algorithm_kurtosis(d, tol = 1e-4)))
  t2 <- system.time(suppressMessages(algorithm_kurtosis(d, tol = 1e-8)))
  expect_lt(t1[["elapsed"]], t2[["elapsed"]])
})
