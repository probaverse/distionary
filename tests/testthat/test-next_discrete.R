test_that("next_discrete_natural works", {
  expect_equal(next_discrete_natural(5, n = 4, include_from = TRUE), 5:8)
  expect_equal(next_discrete_natural(5, n = 4, include_from = FALSE), 6:9)
  expect_equal(next_discrete_natural(5.1, n = 4, include_from = TRUE), 6:9)
  expect_equal(next_discrete_natural(5.1, n = 4, include_from = FALSE), 6:9)
  expect_equal(next_discrete_natural(0, n = 4, include_from = TRUE), 0:3)
  expect_equal(next_discrete_natural(0, n = 4, include_from = FALSE), 1:4)
  expect_error(next_discrete_natural(5.1, n = Inf, include_from = FALSE))
  expect_length(next_discrete_natural(5.1, n = 0, include_from = FALSE), 0)
  expect_length(next_discrete_natural(5.1, n = 0, include_from = TRUE), 0)
})

test_that("prev_discrete_natural works", {
  expect_equal(prev_discrete_natural(5, n = 4, include_from = TRUE), 5:2)
  expect_equal(prev_discrete_natural(5, n = 4, include_from = FALSE), 4:1)
  expect_equal(prev_discrete_natural(5.1, n = 4, include_from = TRUE), 5:2)
  expect_equal(prev_discrete_natural(5.1, n = 4, include_from = FALSE), 5:2)
  expect_equal(prev_discrete_natural(0, n = 4, include_from = TRUE), 0)
  expect_length(prev_discrete_natural(0, n = 4, include_from = FALSE), 0)
  expect_equal(prev_discrete_natural(5, n = Inf, include_from = TRUE), 5:0)
  expect_equal(prev_discrete_natural(5, n = Inf, include_from = FALSE), 4:0)
  expect_equal(prev_discrete_natural(5.1, n = Inf, include_from = TRUE), 5:0)
  expect_equal(prev_discrete_natural(5.1, n = Inf, include_from = FALSE), 5:0)
  expect_length(prev_discrete_natural(5.1, n = 0, include_from = FALSE), 0)
  expect_length(prev_discrete_natural(5.1, n = 0, include_from = TRUE), 0)
  expect_length(prev_discrete_natural(-5.1, n = 4, include_from = TRUE), 0)
  expect_length(prev_discrete_natural(-5.1, n = Inf, include_from = TRUE), 0)
})

test_that("next_discrete norm works", {
  expect_equal(numeric(), next_discrete(dst_norm(1, 2), 1))
  expect_equal(numeric(), next_discrete(dst_norm(12, 1), -2, n = 10))
  expect_equal(numeric(), next_discrete(dst_norm(65, 1), 0, n = Inf))
})

test_that("next_discrete gpd works", {
  expect_equal(numeric(), next_discrete(dst_gpd(2, -4), 1))
  expect_equal(numeric(), next_discrete(dst_gpd(1, 2), 1546, n = 10))
  expect_equal(numeric(), next_discrete(dst_gpd(1, 8), -565, n = Inf))
})

test_that("next_discrete unif works", {
  expect_equal(numeric(), next_discrete(dst_unif(1, 2), 1))
  expect_equal(numeric(), next_discrete(dst_unif(-15, 0), -6, n = 10))
  expect_equal(numeric(), next_discrete(dst_unif(561, 18000), -99, n = Inf))
})

# test_that("next_discrete pois works", {
#   expect_equal(
#     2,
#     next_discrete(dst_pois(1), 1)
#   )
#   expect_equal(
#     0,
#     next_discrete(dst_pois(5456), -2)
#   )
#   expect_equal(
#     1:10 + 55,
#     next_discrete(dst_pois(561), 55.1545645, n = 10)
#   )
#   expect_equal(
#     1,
#     next_discrete(dst_pois(2), 0.465)
#   )
#   expect_equal(
#     0,
#     next_discrete(dst_pois(55), -1.465)
#   )
# })



test_that("next_discrete lnorm works", {
  expect_equal(numeric(), next_discrete(dst_lnorm(1, 2), 1))
  expect_equal(numeric(), next_discrete(dst_lnorm(-15, 2), -16))
  expect_equal(numeric(), next_discrete(dst_lnorm(2, 3), 80))
  expect_equal(numeric(), next_discrete(dst_lnorm(2, 2), 16))
})

