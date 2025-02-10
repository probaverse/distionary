stats_distributions <- list(
  list(
    distribution = dst_bern,
    invalid = list(
      c(prob = -1),
      c(prob = 2)
    ),
    valid = list(
      c(prob = 0.5),
      c(prob = 0.7)
    )
  ),
  list(
    distribution = dst_beta,
    invalid = list(
      c(shape1 = -1, shape2 = 1),
      c(shape1 = 1, shape2 = -1)
    ),
    valid = list(
      c(shape1 = 2, shape2 = 0.5),
      c(shape1 = 0.5, shape2 = 0.5)
    )
  )
)

for (i in seq_along(stats_distributions)) {
  item <- stats_distributions[[i]]

  test_that(paste("Distribution", i, "invalid parameters check."), {
    for (paramset in item$invalid) {
      expect_error(rlang::exec(item$distribution, !!!paramset))
    }
  })

  test_that(paste("Distribution", i, "density / mass lineup with vtype"), {
    d <- rlang::exec(item$distribution, !!!item$valid[[1]])
    v <- vtype(d)
    if (v == "discrete") expect_null(d$density)
    if (v == "continuous") expect_null(d$pmf)
  })

  test_that(paste("Distribution", i, "quantities are valid."), {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      ppties <- names(d)
      ppties <- ppties[ppties != "parameters"]
      # check_
    }
  })
}
