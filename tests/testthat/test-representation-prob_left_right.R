test_that("Probabilities left and right are the cdf and survival function", {
  d <- dst_pois(5)
  expect_equal(eval_prob_left(d, at = 0:3), eval_cdf(d, at = 0:3))
  expect_equal(eval_prob_right(d, at = 0:3), eval_survival(d, at = 0:3))
  expect_equal(
    eval_prob_left(d, at = 0:3, inequality = "strict"),
    eval_cdf(d, at = 0:3, inequality = "strict")
  )
  expect_equal(
    eval_prob_right(d, at = 0:3, inequality = "weak"),
    eval_survival(d, at = 0:3, inequality = "weak")
  )
})

test_that("Matching inequalities split the probability in two", {
  d <- dst_pois(5)
  # At-or-below plus strictly-above is everything, counted once.
  expect_equal(
    eval_prob_left(d, at = 0:3) + eval_prob_right(d, at = 0:3),
    rep(1, 4)
  )
  # Strictly-below plus at-or-above is too.
  expect_equal(
    eval_prob_left(d, at = 0:3, inequality = "strict") +
      eval_prob_right(d, at = 0:3, inequality = "weak"),
    rep(1, 4)
  )
  # The mismatched pairs differ from 1 by exactly the mass on the point.
  expect_equal(
    eval_prob_left(d, at = 0:3) +
      eval_prob_right(d, at = 0:3, inequality = "weak") - 1,
    eval_pmf(d, at = 0:3)
  )
})

test_that("Probabilities left and right can be enframed", {
  d <- dst_pois(5)
  expect_equal(
    enframe_prob_left(d, at = 0:3)[["prob_left"]],
    eval_prob_left(d, at = 0:3)
  )
  expect_equal(
    enframe_prob_right(d, at = 0:3, inequality = "weak")[["prob_right"]],
    eval_prob_right(d, at = 0:3, inequality = "weak")
  )
})

test_that("The old `prob_left()` and `prob_right()` are deprecated", {
  d <- dst_pois(5)
  expect_warning(
    p <- prob_left(d, of = 3, inclusive = TRUE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(p, eval_prob_left(d, at = 3))
  expect_warning(
    p <- prob_left(d, of = 3, inclusive = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(p, eval_prob_left(d, at = 3, inequality = "strict"))
  expect_warning(
    p <- prob_right(d, of = 3, inclusive = TRUE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(p, eval_prob_right(d, at = 3, inequality = "weak"))
  expect_warning(
    p <- prob_right(d, of = 3, inclusive = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(p, eval_prob_right(d, at = 3))
})
