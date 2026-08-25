test_that("a distribution has length 1", {
  expect_identical(length(dst_norm(0, 1)), 1L)
  expect_identical(length(dst_null()), 1L)
  expect_identical(length(dst_pois(3)), 1L)
  expect_identical(
    length(distribution(
      cdf = function(x) stats::punif(x),
      density = function(x) stats::dunif(x),
      .support = continuous(c(0, 1))
    )),
    1L
  )
})

test_that("`is.na()` finds the Null distribution and nothing else", {
  expect_true(is.na(dst_null()))
  expect_false(is.na(dst_norm(0, 1)))
  expect_false(is.na(dst_pois(3)))
  # One answer, not one per property.
  expect_length(is.na(dst_norm(0, 1)), 1L)
  expect_length(is.na(dst_null()), 1L)
})

test_that("`is.na()` is not fooled by a distribution named 'Null'", {
  impostor <- distribution(
    cdf = function(x) stats::punif(x),
    density = function(x) stats::dunif(x),
    .support = continuous(c(0, 1)),
    .name = "Null"
  )
  expect_false(is.na(impostor))
})

test_that("`dst_*()` functions given `NA` give a distribution that is NA", {
  expect_true(is.na(dst_norm(NA, 1)))
  expect_true(is.na(dst_pois(NA)))
})

test_that("`as.list()` gives a list of one distribution", {
  d <- dst_norm(0, 1)
  l <- as.list(d)
  expect_type(l, "list")
  expect_length(l, 1L)
  expect_identical(l[[1]], d)
  expect_null(names(l))
})

test_that("the properties are still reachable", {
  d <- dst_norm(0, 1)
  expect_true(is.function(d[["cdf"]]))
  expect_true("density" %in% names(d))
  expect_equal(eval_property(d, "mean"), 0)
  expect_equal(eval_density(d, at = 0), stats::dnorm(0))
})

test_that("vectors of distributions are lists, and behave elementwise", {
  ds <- list(dst_norm(0, 1), dst_null(), dst_pois(3))
  expect_identical(vapply(ds, is.na, logical(1)), c(FALSE, TRUE, FALSE))
  expect_identical(vapply(ds, length, integer(1)), c(1L, 1L, 1L))
  expect_length(ds, 3L)
})
