test_that("Skewness calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "skewness")) {
        expect_equal(eval_skewness_from_network(d), skewness(d))
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
#   x3 <- ((x - mu) / sigma)^3
#   p <- eval_pmf(distribution, at = x)
#   return(sum(p * x3))
# }
# if (attr(distribution, "name") %in% c(
#   "Negative Binomial", "Poisson", "Geometric"
# )) {
#   to_add <- Inf
#   i <- 0
#   skew <- 0
#   while (to_add > 1e-9) {
#     x <- 0:99 + 100 * i
#     to_add <- sum(eval_pmf(distribution, x) * ((x - mu) / sigma)^3)
#     skew <- skew + to_add
#     i <- i + 1
#   }
#   return(skew)
# }
