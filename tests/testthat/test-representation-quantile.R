test_that("Quantile function calculated thru network matches known vals", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      p <- 1:99 / 100
      if (is_intrinsic(d, "quantile")) {
        if (vtype(d) == "continuous") {
          expect_equal(
            eval_quantile_from_network(d, at = p),
            eval_quantile(d, at = p)
          )
        } else if (pretty_name(d) != "Degenerate") {
          expect_error(eval_quantile_from_network(d, at = 0.4))
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

#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance. --> Copied to `test-representation-quantile.R`.
test_that("Quantile algorithm takes longer for smaller tolerance.", {
  d <- dst_norm(0, 1)
  p <- 1:99 / 100
  t1 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-4))
  t2 <- system.time(eval_quantile_from_network(d, at = p, tol = 1e-8))
  expect_lt(t1[["elapsed"]], t2[["elapsed"]])
})
