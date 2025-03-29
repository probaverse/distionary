test_that("Network is invoked in priority: variance", {
  # First look for `stdev`, then invoke algorithm if not found.
  d <- distribution(
    density = \(x) stats::dnorm(x, sd = 3),
    range = c(-Inf, Inf),
    stdev = 10, # deliberately incorrect
    .vtype = "continuous"
  )
  expect_equal(variance(d), 100)
  expect_equal(eval_variance_from_network(d), 100)
  d$stdev <- NULL
  expect_equal(variance(d), 9)
  expect_equal(eval_variance_from_network(d), 9)
})

test_that("Variance algorithm matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "variance") || is_intrinsic(d, "stdev")) {
        supposed_var <- variance(d)
        if (is.infinite(supposed_var)) {
          supposed_var <- NaN  # To align with numerical integration output.
        }
        if (vtype(d) == "continuous") {
          if (
            pretty_name(d) == "Generalised Extreme Value" &&
            parameters(d)$shape == 0
          ) {
            # GEV with shape = 0 has NaN density for very negative values.
            mu <- mean(d)
            integrand <- \(x) (x - mu)^2 * eval_density(d, at = x)
            expect_equal(
              cubature::hcubature(integrand, -1000, Inf)$integral,
              supposed_var
            )
          } else {
            expect_equal(suppressMessages(algorithm_variance(d)), supposed_var)
          }
        } else if (
          pretty_name(d) %in%
          c("Hypergeometric", "Bernoulli", "Binomial")
        ) {
          expect_error(algorithm_variance(d))
          r <- range(d)
          x <- seq(r[1], r[2], by = 1L)
          mu <- mean(d)
          x2 <- (x - mu)^2
          p <- eval_pmf(d, at = x)
          expect_equal(sum(p * x2), supposed_var)
        } else if (
          pretty_name(d) %in%
          c("Negative Binomial", "Poisson", "Geometric")
        ) {
          expect_error(algorithm_variance(d))
          mu <- mean(d)
          to_add <- Inf
          i <- 0
          v <- 0
          while (to_add > 1e-9) {
            x <- 0:99 + 100 * i
            to_add <- sum(eval_pmf(d, x) * (x - mu)^2)
            v <- v + to_add
            i <- i + 1
          }
          expect_equal(v, supposed_var)
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

test_that("Variance algorithm takes longer for smaller tolerance.", {
  d <- dst_norm(0, 1)
  t1 <- system.time(algorithm_variance(d, tol = 1e-4))
  t2 <- system.time(algorithm_variance(d, tol = 1e-8))
  expect_lt(t1[["elapsed"]], t2[["elapsed"]])
})
