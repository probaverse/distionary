## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distionary)

## -----------------------------------------------------------------------------
# Make a Null distribution.
null <- dst_null()
# Inspect
null

## -----------------------------------------------------------------------------
mean(null)
eval_pmf(null, at = 1:4)

## -----------------------------------------------------------------------------
dst_norm

## -----------------------------------------------------------------------------
# Make a continuous distribution
linear <- distribution(
  parameters = list(a = 1),
  density = function(x) {
    d <- 2 * (1 - x)
    d[x < 0 | x > 1] <- 0
    d
  },
  cdf = function(x) {
    p <- 2 * x * (1 - x / 2)
    p[x < 0] <- 0
    p[x > 1] <- 1
    p
  },
  g = 9.81,
  another_representation = function(x) x^2,
  .vtype = "continuous",
  .name = "My Linear"
)
# Inspect
linear

## -----------------------------------------------------------------------------
eval_cdf(linear, at = c(0.2, 0.5, 0.7))
mean(linear)

## -----------------------------------------------------------------------------
eval_property(linear, "cdf", c(0.2, 0.5, 0.7))
eval_property(linear, "mean")

## -----------------------------------------------------------------------------
eval_property(linear, "another_representation", 1:4)
eval_property(linear, "g")

