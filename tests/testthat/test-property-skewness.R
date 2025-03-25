test_that("Skewness algorithm matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "skewness")) {
        supposed_skew <- skewness(d)
        if (is.infinite(supposed_skew)) {
          supposed_skew <- NaN  # To align with numerical integration output.
        }
        if (vtype(d) == "continuous") {
          expect_equal(
            suppressMessages(eval_skewness_from_network(d)),
            supposed_skew
          )
        } else if (
          pretty_name(d) %in%
          c("Hypergeometric", "Bernoulli", "Binomial")
        ) {
          expect_error(eval_skewness_from_network(d))
          r <- range(d)
          x <- seq(r[1], r[2], by = 1L)
          mu <- mean(d)
          sigma <- stdev(d)
          x3 <- ((x - mu) / sigma)^3
          p <- eval_pmf(d, at = x)
          expect_equal(sum(p * x3), supposed_skew)
        } else if (
          pretty_name(d) %in%
          c("Negative Binomial", "Poisson", "Geometric")
        ) {
          expect_error(eval_skewness_from_network(d))
          mu <- mean(d)
          sigma <- stdev(d)
          to_add <- Inf
          i <- 0
          sk <- 0
          while (to_add > 1e-9) {
            x <- 0:99 + 100 * i
            to_add <- sum(eval_pmf(d, x) * ((x - mu) / sigma)^3)
            sk <- sk + to_add
            i <- i + 1
          }
          expect_equal(sk, supposed_skew)
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


