test_that("dst_pearson3() accepts a negative shape.", {
  expect_no_error(dst_pearson3(0, 1, -1))
  d <- dst_pearson3(2, 1.5, -3)
  expect_equal(vtype(d), "continuous")
  # Reflected about `location`: upper-bounded there.
  expect_equal(range(d), c(-Inf, 2))
})

test_that("Negative shape is the positive shape reflected about location.", {
  location <- 2
  scale <- 1.5
  k <- 3
  neg <- dst_pearson3(location, scale, -k)
  pos <- dst_pearson3(location, scale, k)
  x <- c(-3, -1, 0, 1, 1.9)
  p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  # F_neg(x) = P(2L - Xpos <= x) = P(Xpos >= 2L - x) = S_pos(2L - x).
  expect_equal(
    eval_cdf(neg, at = x),
    eval_survival(pos, at = 2 * location - x)
  )
  expect_equal(
    eval_density(neg, at = x),
    eval_density(pos, at = 2 * location - x)
  )
  # Q_neg(p) = 2L - Q_pos(1 - p).
  expect_equal(
    eval_quantile(neg, at = p),
    2 * location - eval_quantile(pos, at = 1 - p)
  )
})

test_that("Negative-shape moments are sign-aware.", {
  location <- 2
  scale <- 1.5
  k <- 3
  d <- dst_pearson3(location, scale, -k)
  expect_equal(mean(d), location - scale * k) # location + scale * shape
  expect_equal(variance(d), k * scale^2) # abs(shape) * scale^2
  expect_equal(skewness(d), -2 / sqrt(k)) # negated relative to shape > 0
  expect_equal(kurtosis_exc(d), 6 / k)
  expect_equal(stdev(d), scale * sqrt(k))
})

test_that("pdq_pearson3() functions handle a negative shape.", {
  # Reflection identities at the raw-function level.
  expect_equal(
    ppearson3(c(-1, 0, 1), location = 2, scale = 1.5, shape = -3),
    ppearson3(
      2 * 2 - c(-1, 0, 1),
      location = 2, scale = 1.5, shape = 3, lower.tail = FALSE
    )
  )
  expect_equal(
    qpearson3(c(0.2, 0.8), location = 0, scale = 1, shape = -2),
    -qpearson3(1 - c(0.2, 0.8), location = 0, scale = 1, shape = 2)
  )
  # A vector of mixed-sign shapes is handled elementwise.
  res <- ppearson3(0, location = 0, scale = 1, shape = c(-2, 2))
  expect_equal(res[1], 1 - res[2])
})
