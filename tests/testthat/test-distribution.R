#' @srrstats {G5.2} Appropriate error behaviour is tested for all
#' functions explicitly, but warnings are omitted and saved for a future
#' version. --> Copied to test-edge_cases-builtin_dists.R

test_that("Object is a distribution.", {
  expect_true(is_distribution(distribution()))
})

test_that("distribution() edge cases satisfied.", {
  expect_true(is_distribution(distribution()))
  expect_error(distribution(1:10))
  expect_error(distribution(.parameters = "foofy"))
  expect_error(distribution(.parameters = list("foofy")))
  expect_error(distribution(.parameters = c(alpha = 4)))
  expect_error(distribution(.name = c("my", "name", "is")))
  expect_error(distribution(.name = character(0)))
  expect_error(distribution(.vtype = c("my", "name", "is")))
  expect_error(distribution(.vtype = character(0)))
})
