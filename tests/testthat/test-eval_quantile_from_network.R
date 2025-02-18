test_that("Quantile function algorithm works for several distributions", {
  p <- 1:99 / 100
  d <- list(
    dst_beta(3, 5),
    dst_binom(15, 0.2),
    dst_norm(0, 1),
    dst_norm(3.87e287, 1),
    dst_norm(3.87e287, 1e-200),
    dst_nbinom(10, 0.5),
    dst_exp(10),
    dst_gpd(342, 3)
  )
  for (d_ in d) {
    expect_equal(
      distionary:::eval_quantile_from_network(d_, at = p),
      eval_quantile(d_, at = p),
      tolerance = 1e-6
    )
  }
})

test_that("Quantile algorithm handles NA appropriately.", {
  d <- distribution(cdf = pnorm, .vtype = "continuous")
  expect_equal(eval_quantile(d, at = NA), NA_real_)
  expect_equal(eval_quantile(d, at = NaN), NaN)
  expect_length(eval_quantile(d, at = numeric(0L)), 0)
  expect_equal(
    eval_quantile(d, at = c(0.3, NA, NaN)),
    c(stats::qnorm(0.3), NA, NaN)
  )
  psteep <- \(x) pmax(pmin(10 * (x - 30), 1), 0)
  d_steep <- distribution(
    cdf = psteep,
    .vtype = "continuous"
  )
  expect_equal(psteep(eval_quantile(d_steep, at = 0:10 / 10)), 0:10 / 10)
  d_fixed <- distribution(
    cdf = \(x) as.numeric(x >= -1e30),
    .vtype = "continuous"
  )
  expect_equal(eval_quantile(d_fixed, at = 0:10 / 10), rep(-1e30, 11))
  d_flat <- distribution(
    cdf = function(x) {
      res <- rep(0.5, length(x))
      res[x < 0.5] <- pmax(x[x < 0.5], 0)
      res[x > 100] <- pmin(x[x > 100] - 99.5, 1)
      res
    },
    .vtype = "continuous"
  )
  expect_equal(eval_quantile(d_flat, at = 0.5), 0.5)
})
