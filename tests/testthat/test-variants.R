# The variant machinery: how a request for something other than the plain
# representation finds its way to a function.

poisson_pieces <- function(...) {
  distribution(
    cdf = function(x) ppois(x, 5),
    pmf = function(x) dpois(x, 5),
    ...,
    .support = discrete(natural0())
  )
}

test_that("A declared variant is the one that gets called", {
  d <- poisson_pieces(
    quantile = variants(
      function(p) qpois(p, 5),
      right = function(p) rep(-99, length(p))
    )
  )
  expect_equal(eval_quantile(d, at = 0.5), 5)
  expect_equal(eval_quantile(d, at = 0.5, side = "right"), -99)
})

test_that("A plain function provides the canonical variant, and no other", {
  # The stored quantile function is deliberately wrong, so that an answer
  # other than the sentinel proves the variant was derived instead.
  d <- poisson_pieces(quantile = function(p) rep(-99, length(p)))
  expect_equal(eval_quantile(d, at = 0.5), -99)
  expect_equal(eval_quantile(d, at = 0.5, side = "right"), 5)
})

test_that("A variant is derived when the distribution doesn't provide it", {
  d <- poisson_pieces()
  p <- eval_cdf(d, at = 3)
  expect_equal(eval_quantile(d, at = p), 3)
  expect_equal(eval_quantile(d, at = p, side = "right"), 4)
})

test_that("A canonical request reaches a plain function as a bare call", {
  # A function that only accepts one argument would error on anything extra.
  d <- poisson_pieces(quantile = function(p) qpois(p, 5))
  expect_equal(eval_quantile(d, at = 1:9 / 10), qpois(1:9 / 10, 5))
  expect_equal(eval_cdf(d, at = 0:5), ppois(0:5, 5))
})

test_that("Declaring a level no variant accepts is an error", {
  expect_error(
    poisson_pieces(quantile = variants(qnorm, sideways = qnorm)),
    "not a variant of 'quantile'"
  )
})

test_that("Declaring a variant that is not routed is an error", {
  # `definition` asks whether a density exists for this distribution, not
  # which function to call, so there is nothing to declare.
  expect_error(
    distribution(
      cdf = pnorm,
      density = variants(dnorm, strict = dnorm),
      .support = continuous()
    ),
    "no variants to declare"
  )
})

test_that("A representation knows what it was built for", {
  q <- variants(qnorm, right = qnorm, .name = "quantile")
  expect_error(
    distribution(
      cdf = pnorm,
      density = variants(dnorm, .name = "quantile"),
      .support = continuous()
    ),
    "built for 'quantile'"
  )
  expect_s3_class(q, "representation")
  expect_true(is.function(q))
})

test_that("A representation is callable as its canonical variant", {
  q <- variants(qnorm, right = qnorm, .name = "quantile")
  expect_equal(q(0.4), qnorm(0.4))
})

test_that("A representation with no canonical variant says so", {
  q <- variants(right = function(p) rep(-99, length(p)), .name = "quantile")
  expect_error(q(0.4), "provides no canonical variant")
  d <- poisson_pieces(quantile = q)
  # The canonical variant is derived, and the declared one is used.
  expect_equal(eval_quantile(d, at = 0.5), 5)
  expect_equal(eval_quantile(d, at = 0.5, side = "right"), -99)
})

test_that("`variants()` needs at least one function, and functions only", {
  expect_error(variants(), "at least one function")
  expect_error(variants("not a function"), "`.f` must be")
  expect_error(variants(qnorm, right = "not a function"))
})

test_that("`eval_property()` takes a variant, and refuses nonsense ones", {
  d <- poisson_pieces()
  expect_equal(
    eval_property(d, "quantile", 0.5, variant = list(side = "right")),
    5
  )
  d_const <- poisson_pieces(mean = 5)
  expect_error(
    eval_property(d_const, "mean", variant = list(side = "right")),
    "not a function"
  )
  expect_error(
    eval_property(d, "foofy", 1:10, variant = list(side = "right")),
    "cannot derive it"
  )
})

test_that("Variant arguments have to be named", {
  d <- poisson_pieces()
  expect_error(eval_quantile(d, 0.5, "right"))
  expect_error(eval_cdf(d, 3, "strict"))
})

test_that("An unknown variant level is refused at the `eval_` function", {
  d <- poisson_pieces()
  expect_error(eval_quantile(d, at = 0.5, side = "sideways"))
  expect_error(eval_cdf(d, at = 3, inequality = "loose"))
  expect_error(eval_return(d, at = 3, event = "sideways"))
  expect_error(eval_density(d, at = 3, definition = "loose"))
})

test_that("Printing a representation says what it holds", {
  q <- variants(qnorm, right = qnorm, .name = "quantile")
  expect_output(print(q), "representation: quantile")
  expect_output(print(q), "canonical")
  expect_output(print(q), "side = \"right\"")
  expect_output(print(variants(qnorm)), "<representation>")
})

test_that("Which variants exist is read off the `eval_` function", {
  expect_equal(declarable_variants("quantile"), list(side = c("left", "right")))
  expect_equal(canonical_variant("quantile"), list(side = "left"))
  expect_equal(declarable_variants("density"), list())
  expect_equal(declarable_variants("not_a_representation"), list())
})

test_that("A variant records only what departs from the canonical", {
  expect_equal(variant(side = "left", .entry = "quantile"), list())
  expect_equal(
    variant(side = "right", .entry = "quantile"),
    list(side = "right")
  )
})

test_that("A declared variant agrees with the derived one", {
  # The strongest thing to ask of the two routes to a variant: that a
  # distribution declaring one and a distribution leaving it to be derived
  # answer identically, including where the two inverses part company.
  declared <- poisson_pieces(
    quantile = variants(
      function(p) qpois(p, 5),
      right = function(p) {
        q <- qpois(p, 5)
        q + (ppois(q, 5) <= p)
      }
    )
  )
  derived <- poisson_pieces()
  at <- sort(c(1:99 / 100, ppois(0:8, 5)))
  expect_equal(
    eval_quantile(declared, at = at, side = "right"),
    eval_quantile(derived, at = at, side = "right")
  )
  expect_equal(
    eval_quantile(declared, at = at),
    eval_quantile(derived, at = at)
  )
})
