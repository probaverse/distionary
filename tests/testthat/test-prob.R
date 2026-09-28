d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)

test_that("a variable left out is free", {
  expect_equal(prob(d, x < 0), 0.5)
  expect_equal(prob(d, y > c(1, 3, 5)), stats::pnorm(c(1, 3, 5), 1, 2, FALSE))
})

test_that("&, |, and ! combine conditions", {
  skip_if_not_installed("mvtnorm")
  cdf <- eval_bi_cdf(d, 0, 1)
  expect_equal(prob(d, x <= 0 & y <= 1), cdf)
  expect_equal(prob(d, x <= 0 & y > 1), 0.5 - cdf)
  expect_equal(prob(d, x > 0 & y > 1), eval_bi_survival(d, 0, 1))
  expect_equal(prob(d, x > 2 | y > 5), 1 - eval_bi_cdf(d, 2, 5))
  expect_equal(prob(d, !(x > 0 & y > 1)), 1 - eval_bi_survival(d, 0, 1))
  expect_equal(prob(d, 0 < x & x <= 1), stats::pnorm(1) - 0.5)
  quarters <- prob(d, x <= 0 & y <= 1) + prob(d, x <= 0 & y > 1) +
    prob(d, x > 0 & y <= 1) + prob(d, x > 0 & y > 1)
  expect_equal(quarters, 1)
})

test_that("combinations of variables use the `linear` property", {
  # x + y is Normal(1, 7.4); x - y is Normal(-1, 2.6).
  expect_equal(
    prob(d, x + y > 3),
    stats::pnorm(3, 1, sqrt(7.4), lower.tail = FALSE)
  )
  expect_equal(prob(d, x < y), stats::pnorm(0, -1, sqrt(2.6)))
  expect_equal(prob(d, y > x), prob(d, x - y < 0))
  expect_equal(prob(d, 2 * x - 2 * y < 0), prob(d, x < y))
  t3 <- dst_mv_norm(c(a = 0, b = 0, c = 0), diag(3) + 0.3)
  expect_equal(prob(t3, a + b > c), 0.5)
  tt <- dst_bi_t(c(0, 0), c(1, 1), 0.5, 4)
  expect_equal(
    prob(tt, x + y > 1),
    stats::pt(1 / sqrt(3), 4, lower.tail = FALSE)
  )
})

test_that("an event that cannot be evaluated says why", {
  g <- distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  )
  expect_equal(prob(g, x < 0 & y < 1), 0.5 * stats::pexp(1))
  expect_error(prob(g, x < y), "`x - y`")
  expect_error(prob(d, exp(x) > 1), "exp\\(x\\)")
  expect_error(prob(d, x), "is a quantity")
  expect_error(prob(d, x < c(1, 2) & y < c(1, 2, 3)), "lengths")
  expect_error(prob(d, x * c(1, 2) > 0), "single numbers")
})

test_that("finite distributions take any condition", {
  e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  expect_equal(prob(e, a < 2 & b <= 1), 0.25)
  expect_equal(prob(e, a == b), 0.5)
  expect_equal(prob(e, a * b >= 4), 0.5)
  expect_equal(prob(e, a > 1 | b > 1), 0.75)
})

test_that("atoms are handled exactly", {
  p <- dst_pois(3)
  expect_equal(prob(p, x < 2), stats::ppois(1, 3))
  expect_equal(prob(p, 2 <= x & x < 5), stats::ppois(4, 3) - stats::ppois(1, 3))
  expect_equal(
    prob(p, x >= 2 & x != 4),
    stats::ppois(1, 3, lower.tail = FALSE) - stats::dpois(4, 3)
  )
  expect_equal(prob(p, x^2 > 10), stats::ppois(3, 3, lower.tail = FALSE))
  h <- distribution(
    pmf = function(x, y) stats::dpois(x, 2) * stats::dbinom(y, 3, 0.5),
    cdf = function(x, y) stats::ppois(x, 2) * stats::pbinom(y, 3, 0.5),
    .support = support_product(n = discrete(natural0()), k = discrete(0:3))
  )
  expect_equal(
    prob(h, n < 2 & k >= 1),
    stats::ppois(1, 2) * stats::pbinom(0, 3, 0.5, lower.tail = FALSE)
  )
  mx <- distribution(
    cdf = function(x) ifelse(x < 0, 0, 0.3 + 0.7 * stats::pexp(x)),
    density = function(x) ifelse(x < 0, 0, 0.7 * stats::dexp(x)),
    pmf = function(x) ifelse(x == 0, 0.3, 0),
    .support = mixed(discrete = 0, continuous = c(0, Inf))
  )
  expect_equal(prob(mx, x > 0), 0.7)
  expect_equal(prob(mx, x >= 0), 1)
  expect_equal(prob(mx, x == 0), 0.3)
})

test_that("conditioning works on values and on events", {
  expect_equal(
    prob(d, y > 3, given = x == 1),
    stats::pnorm(3, 2.2, 1.6, lower.tail = FALSE)
  )
  expect_equal(
    prob(d, y > 3, given = x == c(1, 1, 2))[c(1, 3)],
    stats::pnorm(3, c(2.2, 3.4), 1.6, lower.tail = FALSE)
  )
  expect_equal(
    prob(d, y > 3, given = x > 1),
    prob(d, x > 1 & y > 3) / prob(d, x > 1)
  )
  e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
  expect_equal(prob(e, b == 1, given = a == 2), 0.5)
  expect_true(is.nan(prob(e, b == 1, given = a == 9)))
  expect_error(prob(d, x < 1, given = x == 1), "fixes to a")
  expect_error(prob(d, TRUE, given = x == 1 & y == 2), "every variable")
})

test_that("values come from the caller, and variables take precedence", {
  threshold <- 1
  expect_equal(prob(d, x < threshold), stats::pnorm(1))
  x <- 5
  expect_equal(prob(d, y < .env$x), stats::pnorm(5, 1, 2))
  expect_equal(prob(d, TRUE), 1)
  expect_identical(is.na(prob(d, x < c(NA, 1))), c(TRUE, FALSE))
  expect_true(is.na(prob(dst_null(), x < 1)))
})

test_that("a univariate distribution's variable is x, or its name", {
  n <- dst_norm(0, 1)
  expect_equal(prob(n, x > 1), stats::pnorm(1, lower.tail = FALSE))
  # The upper tail keeps its precision.
  expect_equal(prob(n, x > 10), stats::pnorm(10, lower.tail = FALSE))
  variables(n) <- "z"
  expect_equal(prob(n, z < 0), 0.5)
})

test_that("giving a combination a value slices the distribution", {
  flows <- dst_mv_norm(c(r1 = 50, r2 = 80), matrix(c(100, 60, 60, 225), 2))
  # r1 given r1 + r2 = s: mean 50 + 160 / 445 * (s - 130).
  s <- c(150, 200, 250)
  m <- 50 + 160 / 445 * (s - 130)
  sd <- sqrt(100 - 160^2 / 445)
  expect_equal(
    prob(flows, r1 > r2, given = r1 + r2 == s),
    stats::pnorm(s / 2, m, sd, lower.tail = FALSE)
  )
  expect_equal(
    prob(flows, r2 <= 120, given = r1 + r2 == 200),
    prob(flows, r1 >= 80, given = r1 + r2 == 200)
  )
  g <- distribution(
    density = function(x, y) stats::dnorm(x) * stats::dexp(y),
    cdf = function(x, y) stats::pnorm(x) * stats::pexp(y),
    .support = support_product(x = continuous(), y = continuous(c(0, Inf)))
  )
  expect_error(prob(g, x > 0, given = x + y == 1), "`linear` property")
})

test_that("commas join conditions, as in filter()", {
  expect_equal(prob(d, x <= 0, y > 1), prob(d, x <= 0 & y > 1))
  expect_equal(prob(d, 0 < x, x <= 1), prob(d, 0 < x & x <= 1))
  expect_equal(
    prob(d, x > 2 | y > 5, x < 3),
    prob(d, (x > 2 | y > 5) & x < 3)
  )
  expect_equal(prob(d), 1)
  expect_error(prob(d, x = 2), "Did you mean `x == ...`")
})

test_that("%in% compares with a set of values", {
  p <- dst_pois(3)
  expect_equal(prob(p, x %in% c(1, 2)), sum(stats::dpois(1:2, 3)))
  expect_equal(prob(p, x %in% c(1, 1, 2)), sum(stats::dpois(1:2, 3)))
  expect_equal(prob(p, x %in% numeric(0)), 0)
  expect_equal(prob(d, x %in% c(0, 1)), 0)
})

test_that("conditions prob() cannot follow are refused, not misread", {
  expect_error(prob(d, is.na(x)), "`is.na\\(\\)`")
  expect_error(prob(d, x > 0 & is.na(y)), "`is.na\\(\\)`")
  expect_error(prob(d, x > 0 && y > 0), "rather than `&&`")
  expect_error(prob(d, ifelse(x > 0, TRUE, FALSE)), "`ifelse\\(\\)`")
  expect_error(prob(d, pmax(x, y) > 2), "`pmax\\(\\)`")
  expect_error(prob(d, y > 3, given = is.na(x)), "`is.na\\(\\)`")
  hide <- function(v) is.na(v)
  expect_error(prob(d, hide(x)), "cannot evaluate")
  # A helper built from arithmetic is followed.
  double <- function(v) 2 * v
  expect_equal(prob(d, double(x) > 0), 0.5)
  # Cancelling a variable out still counts as using it.
  expect_equal(prob(d, x - x < 1), 1)
  expect_equal(prob(d, (x - x) * y < 1), 1)
})

test_that("== and != on quantities without atoms do not multiply the work", {
  m <- dst_mv_norm(c(a = 0, b = 0, c = 0, e = 0), diag(4))
  expect_equal(prob(m, a != 1, b != 1, c != 1, e != 1), 1)
  expect_equal(prob(m, a > 0, b != 1, c != 2, e != 3), 0.5)
  expect_equal(prob(m, a == 1 | b > 0), 0.5)
  # x - y has an atom at 0 when x and y are always equal.
  tied <- dst_mv_norm(c(x = 0, y = 0), matrix(1, 2, 2))
  expect_equal(prob(tied, x == y), 1)
  expect_equal(prob(tied, x != y), 0)
})
