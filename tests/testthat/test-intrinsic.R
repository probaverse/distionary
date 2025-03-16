test_that("Checking for intrinsically defined properties works", {
  d <- dst_norm(0, 1)
  int <- intrinsics(d)
  # Works as intended.
  expect_true(is.character(intrinsics(d)))
  expect_true(length(intrinsics(d)) > 1)
  expect_false(any(is.na(intrinsics(d))))
  expect_equal(is_intrinsic(d, int), rep(TRUE, length(int)))
  expect_equal(is_intrinsic(d, letters), rep(FALSE, length(letters)))
  expect_true(is_intrinsic(d, "cdf"))
  expect_false(is_intrinsic(d, "hazard"))
  # NA and 0-length inputs; non-distributions
  expect_error(intrinsics(unclass(d)))
  expect_error(is_intrinsic(unclass(d), "cdf"))
  expect_equal(
    is_intrinsic(d, append(int, NA)),
    append(rep(TRUE, length(int)), NA)
  )
  expect_equal(is_intrinsic(d, character()), logical())
})
