
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distionary <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/distionary)](https://CRAN.R-project.org/package=distionary)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![R-CMD-check](https://github.com/probaverse/distionary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/probaverse/distionary/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/probaverse/distionary/graph/badge.svg)](https://app.codecov.io/gh/probaverse/distionary)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

distionary:

1.  Makes standard probability distributions available, like Normal,
    Poisson, and empirical distributions.
2.  Provides a framework for you to construct your own distributions.
3.  Evaluates probability distributions, even when not explicitly
    defined in the distribution object.

## Installation

distionary is not on CRAN yet. You can download the development version
from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("vincenzocoia/distionary")
```

## Example

``` r
library(distionary)
```

You can make distributions from standard families found in the `stats`
package, like the Poisson distribution, along with a few others, like
the Generalised Extreme Value (GEV) distribution.

``` r
# Create a Poisson distribution
poisson <- dst_pois(1.5)
# Inspect
poisson
#> Poisson distribution (discrete)
#> --Parameters--
#> lambda 
#>    1.5
```

``` r
# Create a GEV distribution
gev <- dst_gev(-1, 1, 0.2)
# Inspect
gev
#> Generalised Extreme Value distribution (continuous)
#> --Parameters--
#> location    scale    shape 
#>     -1.0      1.0      0.2
```

Distributional representations can be viewed using the `plot()`
function. Here is the GEV density function, for example.

``` r
plot(gev)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Evaluate various distributional representations, such as the density or
quantiles:

``` r
eval_density(gev, at = -4:4)
#> [1] 9.463887e-41 5.572239e-05 1.803427e-01 3.678794e-01 2.240677e-01
#> [6] 1.102761e-01 5.418294e-02 2.788568e-02 1.514427e-02
eval_quantile(poisson, at = c(0.2, 0.5, 0.9))
#> [1] 0 1 3
```

Or, we can enframe the results in a tibble, keeping the input alongside
the output.

``` r
enframe_pmf(poisson, at = 0:4)
#> # A tibble: 5 × 2
#>    .arg    pmf
#>   <int>  <dbl>
#> 1     0 0.223 
#> 2     1 0.335 
#> 3     2 0.251 
#> 4     3 0.126 
#> 5     4 0.0471
```

Evaluate properties of the distributions.

``` r
mean(gev)
#> [1] -0.1788514
skewness(poisson)
#> [1] 0.8164966
range(gev)
#> [1]  -6 Inf
```

You can make your own distribution, too.

``` r
# Make a distribution.
linear <- distribution(
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
  .vtype = "continuous",
  .name = "My Linear"
)
# Inspect
linear
#> My Linear distribution (continuous)
```

Even though only the density and CDF are defining the distribution,
other properties can be evaluated, like its mean and variance.

``` r
mean(linear)
#> [1] 0.3333333
variance(linear)
#> [1] 0.05555556
```

Even other representations can be evaluated, such as quantiles or the
hazard function.

``` r
enframe_quantile(linear, at = c(0.2, 0.5, 0.9))
#> # A tibble: 3 × 2
#>    .arg quantile
#>   <dbl>    <dbl>
#> 1   0.2    0.106
#> 2   0.5    0.293
#> 3   0.9    0.684
eval_hazard(linear, at = c(0.1, 0.2, 0.3))
#> [1] 2.222222 2.500000 2.857143
```

## Code of Conduct

Please note that the distionary project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
