# --- TEST BLOCK ----
# Make sure there's no density for discrete variables, and no
# PMF for continuous variables.
test_that(paste("Distribution", i, "density / mass lineup with vtype"), {
  d <- rlang::exec(item$distribution, !!!item$valid[[1]])
  v <- vtype(d)
  if (v == "discrete") expect_null(d$density)
  if (v == "continuous") expect_null(d$pmf)
})

test_that("Object is a distribution.", {
  expect_true(is_distribution(d))
})
