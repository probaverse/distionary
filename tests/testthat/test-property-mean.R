test_that("Mean calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      # print(d)
      supposed_mean <- mean(d)
      if (is.infinite(supposed_mean)) {
        supposed_mean <- NaN  # For direct comparison to integral output.
      }
      if (is_intrinsic(d, "mean")) {
        if (vtype(d) == "continuous") {
          if (
            pretty_name(d) == "Generalised Extreme Value" &&
            parameters(d)$shape == 0
          ) {
            # The density becomes NaN if x is too small. Manually check.
            integrand <- \(x) x * eval_density(d, at = x)
            expect_equal(
              stats::integrate(integrand, -1000, 0)$value +
                stats::integrate(integrand, 0, Inf)$value,
              supposed_mean
            )
          } else {
            expect_equal(
              suppressMessages(eval_mean_from_network(d)),
              supposed_mean
            )
          }
        } else if (
          pretty_name(d) %in%
          c("Hypergeometric", "Bernoulli", "Binomial")
        ) {
          expect_error(eval_mean_from_network(d))
          r <- range(d)
          x <- seq(r[1], r[2], by = 1L)
          p <- eval_pmf(d, at = x)
          expect_equal(sum(p * x), supposed_mean)
        } else if (
          pretty_name(d) %in%
          c("Negative Binomial", "Poisson", "Geometric")
        ) {
          expect_error(eval_mean_from_network(d))
          to_add <- Inf
          i <- 0
          mean <- 0
          while (to_add > 1e-9) {
            x <- 1:100 + 100 * i
            to_add <- sum(eval_pmf(d, x) * x)
            mean <- mean + to_add
            i <- i + 1
          }
          expect_equal(mean, supposed_mean)
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
