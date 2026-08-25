
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
    .support = mixed(discrete = 1, continuous = c(0, 2))
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

test_that("The right inverse takes the far side of a flat region.", {
  # Support [1, 2] U [4, 5], so the cdf sits at 0.5 right across the gap.
  d <- distribution(
    cdf = function(x) 0.5 * punif(x, 1, 2) + 0.5 * punif(x, 4, 5),
    density = function(x) 0.5 * dunif(x, 1, 2) + 0.5 * dunif(x, 4, 5),
    .support = continuous(c(1, 2), c(4, 5))
  )
  expect_equal(eval_quantile_from_network(d, 0.5, side = "left"), 2)
  expect_equal(eval_quantile_from_network(d, 0.5, side = "right"), 4)
})

test_that("The two inverses split an atom's jump at its endpoints.", {
  d <- dst_pois(3)
  # `p` exactly on top of a jump: the left inverse keeps that atom, the right
  # inverse moves on to the next one. This is the tie that floating point gets
  # wrong if the comparison is left to it.
  p0 <- ppois(0, 3)
  p1 <- ppois(1, 3)
  expect_identical(
    eval_quantile_from_network(d, c(p0, p1), side = "left"), c(0, 1)
  )
  expect_identical(
    eval_quantile_from_network(d, c(p0, p1), side = "right"), c(1, 2)
  )
  # Strictly inside a jump, the two agree.
  expect_equal(eval_quantile_from_network(d, 0.1, side = "left"), 1)
  expect_equal(eval_quantile_from_network(d, 0.1, side = "right"), 1)
})

test_that("The two inverses agree on a strictly increasing cdf.", {
  d <- dst_norm(0, 1)
  p <- 1:99 / 100
  expect_equal(
    eval_quantile_from_network(d, p, side = "left"),
    eval_quantile_from_network(d, p, side = "right"),
    tolerance = 1e-6
  )
})

test_that("`side` defaults to the left inverse, as eval_quantile() uses.", {
  d <- dst_pois(3)
  p <- 1:99 / 100
  expect_identical(
    eval_quantile_from_network(d, p),
    eval_quantile_from_network(d, p, side = "left")
  )
  expect_equal(eval_quantile_from_network(d, p), eval_quantile(d, at = p))
  expect_error(eval_quantile_from_network(d, 0.5, side = "middle"))
})

test_that("Boundary quantiles are the support's ends, whichever side.", {
  d <- dst_pois(3)
  expect_equal(eval_quantile_from_network(d, c(0, 1), side = "left"),
               c(0, Inf))
  expect_equal(eval_quantile_from_network(d, c(0, 1), side = "right"),
               c(0, Inf))
  b <- dst_unif(2, 7)
  expect_equal(eval_quantile_from_network(b, c(0, 1), side = "right"),
               c(2, 7))
})

test_that("Boundary quantiles never reach the inverter.", {
  # `eval_quantile()` settles 0 and 1 from the support, so a cdf that would
  # blow up if it were called is never called for them.
  exploding <- distribution(
    cdf = function(x) stop("the inverter should not have run"),
    density = stats::dnorm,
    .support = continuous(c(-4, 9))
  )
  expect_equal(eval_quantile(exploding, at = c(0, 1)), c(-4, 9))
})

test_that("The Null distribution still answers quantiles with NA.", {
  # It is the one distribution with no support, and it brings its own
  # quantile function rather than going near the inverter.
  n <- dst_null()
  expect_null(support(n))
  expect_identical(eval_quantile(n, at = c(0, 0.5, 1)), rep(NA_real_, 3))
})

test_that("At the top of a jump, the right inverse lands on the atom.", {
  # An atom at 0 with density resuming immediately after it, so nothing flat
  # follows the jump. `Q+(F(0))` is 0 exactly, not 0 plus a tolerance.
  p0 <- 0.3
  rate <- 1 / 5
  mx <- distribution(
    cdf = function(x) ifelse(x < 0, 0, p0 + (1 - p0) * stats::pexp(x, rate)),
    density = function(x) ifelse(x <= 0, 0, (1 - p0) * stats::dexp(x, rate)),
    pmf = function(x) ifelse(x == 0, p0, 0),
    .support = mixed(discrete = 0, continuous = c(0, Inf))
  )
  expect_identical(
    eval_quantile_from_network(mx, p0, side = "right"), 0
  )
  expect_identical(
    eval_quantile_from_network(mx, p0, side = "left"), 0
  )
  # Strictly inside the jump both inverses give the atom, as the cdf skips
  # that level entirely.
  expect_identical(
    eval_quantile_from_network(mx, 0.15, side = "right"), 0
  )
})

test_that("A gap after the atom still sends the right inverse onward.", {
  # The contrast: a Poisson has nothing between its atoms, so the cdf is flat
  # across the gap and the two inverses take opposite ends of it.
  d <- dst_pois(3)
  p1 <- ppois(1, 3)
  expect_identical(eval_quantile_from_network(d, p1, side = "left"), 1)
  expect_identical(eval_quantile_from_network(d, p1, side = "right"), 2)
})


test_that("The quantile function takes either inverse of the cdf", {
  d <- dst_pois(5)
  p <- eval_cdf(d, at = 3)
  # Between two atoms the cdf is flat, so the inverses land either side.
  expect_equal(eval_quantile(d, at = p), 3)
  expect_equal(eval_quantile(d, at = p, side = "right"), 4)
  # Inside an atom's jump there is no flat stretch to choose an end of.
  p_inside <- (eval_cdf(d, at = 2) + eval_cdf(d, at = 3)) / 2
  expect_equal(eval_quantile(d, at = p_inside), 3)
  expect_equal(eval_quantile(d, at = p_inside, side = "right"), 3)
})

test_that("The two inverses agree on a strictly increasing cdf", {
  d <- dst_norm(0, 1)
  expect_equal(
    eval_quantile(d, at = 1:9 / 10, side = "right"),
    eval_quantile(d, at = 1:9 / 10),
    tolerance = 1e-6
  )
})

test_that("`side` does not apply at the ends of the support", {
  d <- dst_unif(0, 4)
  expect_equal(eval_quantile(d, at = c(0, 1), side = "right"), c(0, 4))
  expect_equal(eval_quantile(d, at = c(0, 1)), c(0, 4))
  n <- dst_norm(0, 1)
  expect_equal(eval_quantile(n, at = c(0, 1), side = "right"), c(-Inf, Inf))
})

test_that("`side` carries through to `enframe_quantile()`", {
  d <- dst_pois(5)
  p <- eval_cdf(d, at = 3)
  expect_equal(
    enframe_quantile(d, at = p, side = "right")[["quantile"]],
    eval_quantile(d, at = p, side = "right")
  )
})
