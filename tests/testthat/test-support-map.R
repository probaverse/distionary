test_that("an affine map of the line is a singular line", {
  line <- support_affine(
    continuous(),
    shift = c(x = 50, y = 50),
    matrix = rbind(1, -1)
  )
  expect_s3_class(line, "support_affine")
  expect_identical(dimension(line), 2L)
  expect_identical(variables(line), c("x", "y"))
  expect_identical(line[["rank"]], 1L)
  expect_identical(vtype_of_support(line), "singular")
  expect_identical(support_marginal(line, 2L), continuous())
})

test_that("affine margins are the scaled regions added together", {
  seg <- support_affine(
    continuous(c(0, 1)),
    shift = c(x = 50, y = 50, z = 0),
    matrix = rbind(1, -1, 0)
  )
  expect_identical(support_marginal(seg, 1L), continuous(c(50, 51)))
  expect_identical(support_marginal(seg, 2L), continuous(c(49, 50)))
  expect_identical(support_marginal(seg, 3L), discrete(0))
  box <- support_affine(
    support_product(continuous(c(0, 1)), continuous(c(0, 2))),
    shift = c(a = 0, b = 0),
    matrix = rbind(c(1, 1), c(1, -1))
  )
  expect_identical(vtype_of_support(box), "continuous")
  expect_identical(support_marginal(box, 1L), continuous(c(0, 3)))
  expect_identical(support_marginal(box, 2L), continuous(c(-2, 1)))
  # Projecting onto several variables keeps the map.
  xz <- support_marginal(seg, c(1L, 3L))
  expect_s3_class(xz, "support_affine")
  expect_identical(variables(xz), c("x", "z"))
})

test_that("an affine map of finitely many points gives points", {
  s <- support_affine(discrete(1:3), shift = c(0, 1), matrix = rbind(1, 2))
  expect_s3_class(s, "support_points")
  expect_equal(s[["points"]][[2L]], c(3, 5, 7))
  expect_error(
    support_affine(
      discrete(natural0()),
      shift = c(0, 0),
      matrix = rbind(1, 1)
    ),
    "infinitely many atoms"
  )
  expect_error(
    support_affine(continuous(), shift = c(0, 0), matrix = rbind(1, 1, 1)),
    "one row per entry"
  )
})

test_that("a general map states its margins", {
  tri <- support_map(
    support_product(x = continuous(c(0, Inf)), z = continuous(c(0, 1))),
    fun = function(x, z) list(x, x * z),
    margins = list(x = continuous(c(0, Inf)), y = continuous(c(0, Inf)))
  )
  expect_identical(vtype_of_support(tri), "continuous")
  expect_identical(variables(tri), c("x", "y"))
  expect_identical(support_marginal(tri, 2L), continuous(c(0, Inf)))
  expect_equal(tri[["fun"]](2, 0.5), list(2, 1))
  expect_output(print(tri), "image of a map")
})

test_that("mapped supports sit inside products", {
  line <- support_affine(
    continuous(),
    shift = c(x = 0, y = 0),
    matrix = rbind(1, 1)
  )
  s <- support_product(line, w = discrete(0:1))
  expect_identical(variables(s), c("x", "y", "w"))
  expect_identical(vtype_of_support(s), "mixed")
  expect_identical(
    vtype_of_support(support_product(line, w = continuous())),
    "singular"
  )
  expect_s3_class(support_marginal(s, 1:2), "support_affine")
  expect_null(enumerate_points(s))
})
