test_that("All dists are being tested thru test_distributions object", {
  all_namespace <- ls(getNamespace("distionary"))
  dst_namespace <- all_namespace[grep("^dst_", all_namespace)]
  dst_namespace <- dst_namespace[dst_namespace != "dst_null"]
  dst_tested <- vapply(
    test_distributions, function(x) x[[1]], FUN.VALUE = character(1L)
  )
  dst_tested <- unname(dst_tested)
  expect_equal(sort(dst_tested), sort(dst_namespace))
})
