
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
        } else if (pretty_name(d) == "Finite") {
          expect_error(eval_quantile_from_network(d, at = 0.4))
          support <- parameters(d)$outcomes
          x <- support[1] - 1
          q_derived <- numeric(0L)
          j <- 0
          for (i in seq_along(p)) {
            p_ <- p[i]
            below <- eval_cdf(d, at = x) < p_
            while (below) {
              j <- j + 1
              x <- support[j]
              below <- eval_cdf(d, at = x) < p_
            }
            q_derived[i] <- x
          }
          expect_equal(
            q_derived,
            eval_quantile(d, at = p)
          )
        } else if (pretty_name(d) != "Degenerate") {
          expect_error(eval_quantile_from_network(d, at = 0.4))
          # Distributions in distionary start at 0 at least.
          x <- -1
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
