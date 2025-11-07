# Evaluate a Distribution

``` r
library(distionary)
```

This vignette covers the second goal of `distionary`: to evaluate
probability distributions, even when that property is not specified in
the distribution’s definition.

## Distributional Representations

A *distributional representation* is a mathematical function that
completely defines a probability distribution. Unlike a simple property
(such as the mean or variance), a representation contains enough
information that any other property or representation can be calculated
from it.

The key innovation in `distionary` is that these representations are
interconnected through a network of relationships, allowing you to
specify a distribution using any available representation and
automatically derive others as needed. For example, if you specify only
a CDF, `distionary` can compute the quantile function, mean, variance,
and other properties.

Here is a list of representations recognised by `distionary`, and the
functions for accessing them.

| Representation                   | `distionary` Functions                                                                                                                                        |
|----------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Cumulative Distribution Function | [`eval_cdf()`](https://distionary.probaverse.com/reference/cdf.md), [`enframe_cdf()`](https://distionary.probaverse.com/reference/cdf.md)                     |
| Survival Function                | [`eval_survival()`](https://distionary.probaverse.com/reference/survival.md), [`enframe_survival()`](https://distionary.probaverse.com/reference/survival.md) |
| Quantile Function                | [`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md), [`enframe_quantile()`](https://distionary.probaverse.com/reference/quantile.md) |
| Hazard Function                  | [`eval_hazard()`](https://distionary.probaverse.com/reference/hazard.md), [`enframe_hazard()`](https://distionary.probaverse.com/reference/hazard.md)         |
| Cumulative Hazard Function       | [`eval_chf()`](https://distionary.probaverse.com/reference/chf.md), [`enframe_chf()`](https://distionary.probaverse.com/reference/chf.md)                     |
| Probability density Function     | [`eval_density()`](https://distionary.probaverse.com/reference/density.md), [`enframe_density()`](https://distionary.probaverse.com/reference/density.md)     |
| Probability mass Function (PMF)  | [`eval_pmf()`](https://distionary.probaverse.com/reference/pmf.md), [`enframe_pmf()`](https://distionary.probaverse.com/reference/pmf.md)                     |
| Odds Function                    | [`eval_odds()`](https://distionary.probaverse.com/reference/odds.md), [`enframe_odds()`](https://distionary.probaverse.com/reference/odds.md)                 |
| Return Level Function            | [`eval_return()`](https://distionary.probaverse.com/reference/return.md), [`enframe_return()`](https://distionary.probaverse.com/reference/return.md)         |

All representations can either be accessed by the `eval_*()` family of
functions, providing a vector of the evaluated representation.

``` r
d1 <- dst_geom(0.6)
eval_pmf(d1, at = 0:5)
#> [1] 0.600000 0.240000 0.096000 0.038400 0.015360 0.006144
```

Alternatively, the `enframe_*()` family of functions provides the
results in a tibble or data frame paired with the inputs, useful in a
data wrangling workflow.

``` r
enframe_pmf(d1, at = 0:5)
#> # A tibble: 6 × 2
#>    .arg     pmf
#>   <int>   <dbl>
#> 1     0 0.6    
#> 2     1 0.24   
#> 3     2 0.096  
#> 4     3 0.0384 
#> 5     4 0.0154 
#> 6     5 0.00614
```

The `enframe_*()` functions allow for insertion of multiple
distributions, placing a column for each distribution. The column names
can be changed in three ways:

1.  The input column `.arg` can be renamed with the `arg_name` argument.
2.  The `pmf` prefix on the evaluation columns can be changed with the
    `fn_prefix` argument.
3.  The distribution names can be changed by assigning name-value pairs
    for the input distributions.

Let’s practice this with the addition of a second distribution.

``` r
d2 <- dst_geom(0.4)
enframe_pmf(
  model1 = d1, model2 = d2, at = 0:5,
  arg_name = "num_failures", fn_prefix = "probability"
)
#> # A tibble: 6 × 3
#>   num_failures probability_model1 probability_model2
#>          <int>              <dbl>              <dbl>
#> 1            0            0.6                 0.4   
#> 2            1            0.24                0.24  
#> 3            2            0.096               0.144 
#> 4            3            0.0384              0.0864
#> 5            4            0.0154              0.0518
#> 6            5            0.00614             0.0311
```

## Drawing a random sample

To draw a random sample from a distribution, use the
[`realise()`](https://distionary.probaverse.com/reference/realise.md) or
[`realize()`](https://distionary.probaverse.com/reference/realise.md)
function:

``` r
set.seed(42)
realise(d1, n = 5)
#> [1] 0 0 0 0 0
```

You can read this call as “realise distribution `d` five times”. By
default, `n` is set to 1, so that realising converts a distribution to a
numeric draw:

``` r
realise(d1)
#> [1] 0
```

While random sampling falls into the same family as the `p*/d*/q*/r*`
functions from the `stats` package (e.g.,
[`rnorm()`](https://rdrr.io/r/stats/Normal.html)), this function is not
a distributional representation, hence does not have a `eval_*()` or
`enframe_*()` counterpart. This is because it’s impossible to perfectly
describe a distribution based on a sample.

## Properties of Distributions

`distionary` distinguishes between *distributional representations*
(which fully define a distribution) and *distributional properties*
(which are characteristics that can be computed from representations).

A distribution *property* is any measurable characteristic that can be
calculated from a distribution’s representation. Unlike representations,
properties do not contain enough information to fully reconstruct the
distribution. For example, knowing the mean and variance of a
distribution doesn’t tell you whether it’s a Normal, Gamma, or some
other distribution family. Properties include statistical moments and
other summary measures.

Below is a table of the properties incorporated in `distionary`, and the
corresponding functions for accessing them.

| Property           | `distionary` Function                                                      |
|--------------------|----------------------------------------------------------------------------|
| Mean               | [`mean()`](https://rdrr.io/r/base/mean.html)                               |
| Median             | [`median()`](https://rdrr.io/r/stats/median.html)                          |
| Variance           | [`variance()`](https://distionary.probaverse.com/reference/moments.md)     |
| Standard Deviation | [`sd()`](https://rdrr.io/r/stats/sd.html)                                  |
| Skewness           | [`skewness()`](https://distionary.probaverse.com/reference/moments.md)     |
| Excess Kurtosis    | [`kurtosis_exc()`](https://distionary.probaverse.com/reference/moments.md) |
| Kurtosis           | [`kurtosis()`](https://distionary.probaverse.com/reference/moments.md)     |
| Range              | [`range()`](https://rdrr.io/r/base/range.html)                             |

Here’s the mean and variance of our original distribution.

``` r
mean(d1)
#> [1] 0.6666667
variance(d1)
#> [1] 1.111111
```
