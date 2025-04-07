test_that("Return levels match some known values", {
  expect_equal(eval_return(dst_unif(0, 1), at = 1), 0)
  expect_equal(eval_return(dst_unif(0, 1), at = 2), 0.5)
  expect_equal(eval_return(dst_unif(0, 1), at = 10), 0.9)
  expect_equal(eval_return(dst_unif(0, 1), at = Inf), 1)
  expect_equal(
    suppressWarnings(eval_return(dst_unif(0, 1), at = c(-1, 0.5))),
    c(NaN, NaN)
  )
})
