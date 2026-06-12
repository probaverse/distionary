
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
          # The network now resolves atoms exactly (no longer disabled).
          expect_equal(eval_quantile_from_network(d, at = p), q_derived)
          expect_equal(eval_quantile(d, at = p), q_derived)
        } else if (pretty_name(d) != "Degenerate") {
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
          # The network now resolves atoms exactly (no longer disabled).
          expect_equal(eval_quantile_from_network(d, at = p), q_derived)
          expect_equal(eval_quantile(d, at = p), q_derived)
        }
      }
    }
  }
})

test_that("Network quantiles snap to atoms exactly for discrete dists", {
  d <- dst_pois(2.3)
  p <- 1:99 / 100
  q <- eval_quantile_from_network(d, at = p)
  # Every quantile of a discrete distribution lands on an atom, here a
  # non-negative integer, returned exactly (not a nearby floating-point value).
  expect_identical(q, as.numeric(round(q)))
  expect_equal(q, eval_quantile(d, at = p))
})

test_that("Network quantiles of a mixed distribution snap only on the jump", {
  # Atom of mass 0.4 at x = 1, plus a Uniform(0, 2) carrying the other 0.6:
  #   F(x) = 0.3 x          on [0, 1)   (so F(1^-) = 0.3)
  #   F(x) = 0.3 x + 0.4    on [1, 2]   (so F(1) = 0.7)
  # Thus p in (0.3, 0.7] inverts to exactly 1; below and above it the quantile
  # lies in the continuous part.
  cdf <- function(x) pmin(pmax(x / 2, 0), 1) * 0.6 + 0.4 * (x >= 1)
  d <- distribution(
    cdf = cdf,
    pmf = function(x) ifelse(x == 1, 0.4, 0),
    .support = mixed(atoms = 1, continuous = c(0, 2))
  )
  p_below <- c(0.1, 0.2)
  p_atom <- c(0.31, 0.5, 0.7)
  p_above <- c(0.71, 0.9)
  expect_equal(eval_quantile_from_network(d, at = p_below), p_below / 0.3)
  expect_true(all(eval_quantile_from_network(d, at = p_atom) == 1))
  expect_equal(
    eval_quantile_from_network(d, at = p_above), (p_above - 0.4) / 0.3
  )
})

test_that("Network quantiles take boundary p from the support hull", {
  d <- suppressWarnings(
    distribution(cdf = pnorm, .support = continuous(c(-Inf, Inf)))
  )
  expect_equal(eval_quantile_from_network(d, at = c(0, 1)), c(-Inf, Inf))
  db <- dst_binom(10, 0.3)
  expect_equal(eval_quantile_from_network(db, at = c(0, 1)), c(0, 10))
  expect_identical(eval_quantile_from_network(d, at = NA_real_), NA_real_)
})
