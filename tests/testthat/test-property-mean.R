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

# if (
#   attr(distribution, "name") %in%
#   c("Hypergeometric", "Bernoulli", "Binomial")
# ) {
#   # This case is for double-checking the moments supplied for these
#   # distributions, and will be included until discretes handling is
#   # implemented.
#   r <- range(distribution)
#   x <- seq(r[1], r[2], by = 1L)
#   p <- eval_pmf(distribution, at = x)
#   return(sum(p * x))
# } else if (
#   attr(distribution, "name") %in%
#   c("Negative Binomial", "Poisson", "Geometric")
# ) {
#   to_add <- Inf
#   i <- 0
#   mean <- 0
#   while (to_add > 1e-9) { # Tolerance built-in because only used in tests.
#     x <- 1:100 + 100 * i
#     to_add <- sum(eval_pmf(distribution, x) * x)
#     mean <- mean + to_add
#     i <- i + 1
#   }
#   return(mean)
# }
