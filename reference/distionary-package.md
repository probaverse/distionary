# distionary: Create and Evaluate Probability Distributions

Create and evaluate probability distribution objects from a variety of
families or define custom distributions. Automatically compute
distributional properties, even when they have not been specified. This
package supports statistical modeling and simulations, and forms the
core of the probaverse suite of R packages.

## Overview

The distionary package provides a comprehensive framework for working
with probability distributions in R. With distionary, you can:

1.  Specify probability distributions from common families or create
    custom distributions.

2.  Evaluate distributional properties and representations.

3.  Access distributional calculations even when they're not directly
    specified.

The main purpose of distionary is to implement a distribution object
that powers the wider [probaverse ecosystem](https://probaverse.com) for
making probability distributions that are representative of your data.

## Creating Distributions

Use the `dst_*()` family of functions to create distributions from
common families:

- [`dst_norm()`](https://distionary.probaverse.com/reference/dst_norm.md),
  [`dst_exp()`](https://distionary.probaverse.com/reference/dst_exp.md),
  [`dst_unif()`](https://distionary.probaverse.com/reference/dst_unif.md),
  etc. are some continuous distributions.

- [`dst_pois()`](https://distionary.probaverse.com/reference/dst_pois.md),
  [`dst_binom()`](https://distionary.probaverse.com/reference/dst_binom.md),
  [`dst_geom()`](https://distionary.probaverse.com/reference/dst_geom.md),
  etc. are some discrete distributions.

- [`dst_empirical()`](https://distionary.probaverse.com/reference/dst_empirical.md)
  is useful for creating a non-parametric distribution from data.

You can also make your own distribution using the
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
function, which allows you to specify any combination of distributional
representations and properties. For this version of distionary, the CDF
and density/PMF are required in order to access all functionality.

## Evaluating Distributions

A distribution's *representations* are functions that fully describe the
distribution. They can be accessed with the `eval_*()` functions. For
example,
[`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md) and
[`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md)
invoke the distribution's cumulative distribution function (CDF) and
quantile function.

Other properties of the distribution can be calculated by functions of
the property's name, such as
[`mean()`](https://rdrr.io/r/base/mean.html) and
[`range()`](https://rdrr.io/r/base/range.html).

## Random Samples

Generate random samples from a distribution using
[`realise()`](https://distionary.probaverse.com/reference/realise.md).

## Getting Started

New users should start with the package vignettes:

- `vignette("specify", package = "distionary")` - Learn how to specify
  distributions.

- [`vignette("evaluate", package = "distionary")`](https://distionary.probaverse.com/articles/evaluate.md) -
  Learn how to evaluate distributions.

## See also

Useful links:

- <https://distionary.probaverse.com/>

- <https://github.com/probaverse/distionary>

- Report bugs at <https://github.com/probaverse/distionary/issues>

## Author

**Maintainer**: Vincenzo Coia <vincenzo.coia@gmail.com> \[copyright
holder\]

Other contributors:

- Amogh Joshi \[contributor\]

- Shuyi Tan \[contributor\]

- Zhipeng Zhu \[contributor\]

## Examples

``` r
# Create a Poisson distribution.
poisson <- dst_pois(lambda = 1.5)
poisson
#> Poisson distribution (discrete) 
#> --Parameters--
#> lambda 
#>    1.5 

# Evaluate the probability mass function.
eval_pmf(poisson, at = 0:4)
#> [1] 0.22313016 0.33469524 0.25102143 0.12551072 0.04706652
plot(poisson)


# Get distribution properties.
mean(poisson)
#> [1] 1.5
variance(poisson)
#> [1] 1.5

# Create a continuous distribution (Normal).
normal <- dst_norm(mean = 0, sd = 1)

# Evaluate quantiles.
eval_quantile(normal, at = c(0.025, 0.5, 0.975))
#> [1] -1.959964  0.000000  1.959964

# Create a custom distribution.
my_dist <- distribution(
  density = function(x) ifelse(x >= 0 & x <= 1, 2 * (1 - x), 0),
  cdf = function(x) ifelse(x >= 0 & x <= 1, 1 - (1 - x)^2, 0),
  .vtype = "continuous",
  .name = "Linear"
)
plot(my_dist)

plot(my_dist, "cdf")


# Even without specifying all properties, they can still be computed.
mean(my_dist)
#> [1] 0.3333333
```
