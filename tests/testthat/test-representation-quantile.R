test_that("Quantile function calculated thru network matches known vals.", {
  for (item in test_distributions) {
    i <- i + 1
    cat("\n---- ", i, "\n")
    j <- 0
    for (paramset in item$valid) {
      j <- j + 1
      cat(j, " ")
      d <- rlang::exec(item$distribution, !!!paramset)
      p <- 1:99 / 100
      if (is_intrinsic(d, "quantile")) {
        if (vtype(d) == "continuous") {
          expect_equal(
            eval_quantile_from_network(d, at = p),
            eval_quantile(d, at = p)
          )
        } else if (pretty_name(d) != "Degenerate") {
          x <- -1 # All distributions in this version start at 0 at least.
          q_derived <- numeric(0L)
          for (i in seq_along(p)) {
            p_ <- p[i]
            below <- eval_cdf(d, at = x) < p_
            while (below) {
              x <- x + 1
              below <- eval_cdf(d, at = x) < p_
            }
            q_derived[i] <- x
          }
          expect_equal(
            q_derived,
            eval_quantile(d, at = p)
          )
        }
      }
    }
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

#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance.
test_that("Quantile algorithm takes longer for smaller tolerance.", {
  d <- dst_norm(0, 1)
  p <- 1:99 / 100
  t1 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-4))
  t2 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-8))
  expect_lt(t1[["elapsed"]], t2[["elapsed"]])
})
