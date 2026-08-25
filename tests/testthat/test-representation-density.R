# Validate density can be recovered from CDF
#
# Systematic testing uses the `test_distributions` object.
# To modify it, see `data-raw/test_distributions.R`.
# To update it, run `Rscript data-raw/test_distributions.R`.



test_that("Built-in density functions match cdf", {
  for (item in test_distributions) {
    for (paramset in item$valid) {
      d <- rlang::exec(item$distribution, !!!paramset)
      if (is_intrinsic(d, "density")) {
        expect_false(is_intrinsic(d, "pmf"))
        rng <- range(d)
        p <- 1:99 / 100
        x <- eval_quantile(d, at = p)
        dens_fun <- representation_as_function(d, "density")
        increments <- vapply(
          seq_along(x[-1L]),
          function(t) {
            stats::integrate(dens_fun, lower = x[t], upper = x[t + 1])$value
          },
          FUN.VALUE = numeric(1L)
        )
        cdf_derived <- p[1] + append(0, cumsum(increments))
        expect_equal(cdf_derived, eval_cdf(d, at = x))
      }
    }
  }
})


# Half the probability sits on a single point, the rest is spread over
# (0, 1): a distribution with a density on part of its support and none of
# it, which is where the two definitions part company.
half_atom <- function() {
  distribution(
    cdf = function(x) {
      p <- 0.5 + 0.5 * x
      p[x < 0] <- 0
      p[x > 1] <- 1
      p
    },
    density = function(x) {
      d <- rep(0.5, length(x))
      d[x < 0 | x > 1] <- 0
      d
    },
    pmf = function(x) {
      0.5 * (x == 0)
    },
    .support = mixed(discrete = 0, continuous = c(0, 1)),
    .name = "Half atom"
  )
}

test_that("The strict definition of a density needs a continuous variable", {
  expect_equal(
    eval_density(dst_norm(0, 1), at = -2:2, definition = "strict"),
    dnorm(-2:2)
  )
  expect_error(
    eval_density(dst_pois(5), at = 0:3, definition = "strict"),
    "has no density function"
  )
  expect_error(
    eval_density(half_atom(), at = 0.5, definition = "strict"),
    "has no density function"
  )
})

test_that("The extended definition reads a density off the cdf", {
  # A discrete distribution has no density, but its cdf has a derivative:
  # flat between the atoms, and undefined where it jumps.
  expect_equal(eval_density(dst_pois(5), at = c(0.5, 1.5)), c(0, 0))
  expect_equal(eval_density(dst_pois(5), at = c(1, 2)), c(NaN, NaN))
  expect_equal(eval_density(dst_pois(5), at = c(NA, 0.5)), c(NA, 0))
  expect_equal(eval_density(half_atom(), at = 0.5), 0.5)
  expect_equal(
    eval_density(dst_norm(0, 1), at = -2:2, definition = "extended"),
    eval_density(dst_norm(0, 1), at = -2:2)
  )
})

test_that("The definition carries through to `enframe_density()`", {
  d <- dst_norm(0, 1)
  expect_equal(
    enframe_density(d, at = -2:2, definition = "strict")[["density"]],
    eval_density(d, at = -2:2)
  )
  expect_error(
    enframe_density(dst_pois(5), at = 0:3, definition = "strict"),
    "has no density function"
  )
})
