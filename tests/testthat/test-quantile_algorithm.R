# d <- mix(dst_unif(0, 5), dst_empirical(c(2, 3, 3, 4, 6)))
# # plot(d, "cdf", from = -1, to = 7, n = 1001)
# cdf <- get_cdf(d)
# tau <- c(
#   0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, NA, NaN, 0.55, 0.6,
#   0.9, 1
# )
# ans <- c(1, 1.5, 2, 2, 2, 2.5, 3, 3, NA, NaN, 3, 3, 5, 6)
# tau_left <- c(-Inf, -100, 0)
# tau_right <- c(100, Inf)
# ans_left <- c(-Inf, -Inf, -Inf)
# ans_right <- c(Inf, Inf)
# set.seed(1)
# permute <- sample(1:length(tau))
#
# test_that("quantiles by inversion of cdf works", {
#   expect_equal(
#     sapply(tau, function(x) {
#       left_inverse(cdf,
#         at = x, low = -1, high = 7,
#         tol = 1e-6, maxiter = 1000
#       )
#     }),
#     ans
#   )
#   expect_equal(eval_quantile(d, at = tau), ans)
#   expect_equal(eval_quantile(d, at = tau[permute]), ans[permute])
#   expect_equal(eval_quantile(d, at = tau_left), ans_left)
#   expect_equal(eval_quantile(d, at = tau_right), ans_right)
#   expect_equal(
#     eval_quantile(d, at = c(tau_right, tau_left, tau)),
#     c(ans_right, ans_left, ans)
#   )
#   expect_equal(eval_quantile(d, at = 0.5), 3)
#   expect_identical(eval_quantile(d, at = numeric(0L)), numeric(0L))
#   expect_identical(eval_quantile(d, at = NA_real_), NA_real_)
#   expect_identical(eval_quantile(d, at = NaN), NaN)
#   expect_identical(
#     eval_quantile(d, at = c(NA_real_, NA_real_)),
#     c(NA_real_, NA_real_)
#   )
# })
#
# d2 <- mix(dst_norm(0, 1), dst_norm(4, 1))
#
# test_that("Quantile algorithm works with no discontinuities", {
#   expect_equal(eval_quantile(d2, at = 0.5), 2)
#   # eval_quantile(d2, at = 1:9/10)
#   expect_equal(
#     eval_quantile(d2, at = c(0, 0.5, NA, 1, 1.3)),
#     c(-Inf, 2, NA, Inf, Inf)
#   )
# })
