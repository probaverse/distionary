# Distribution Objects

Distribution Objects

## Usage

``` r
distribution(..., .vtype = NULL, .name = NULL, .parameters = list())

is_distribution(object)

is.distribution(object)
```

## Arguments

- ...:

  Name-value pairs for defining the distribution.

- .vtype:

  The variable type, typically "discrete" or "continuous". Can be any
  character vector of length 1, but is converted to lowercase with
  [`tolower()`](https://rdrr.io/r/base/chartr.html) for compliance with
  known types.

- .name:

  A name to give to the distribution. Can be any character vector of
  length 1.

- .parameters:

  A named list with one entry per distribution parameter, each of which
  can be any data type. In this version of distionary, these parameters
  are only stored for the benefit of the user to know what distribution
  they are working with; the code never looks at these parameters to
  inform its calculations. This is anticipated to change in a future
  version of distionary.

- object:

  Object to be tested

## Value

A distribution object.

## Details

Currently, the CDF (`cdf`) is required to be specified, along with the
PMF (`pmf`) for discrete distributions and density (`density`) for
continuous distributions. Otherwise, the full extent of distribution
properties will not be accessible.

A distributional representation is a function that fully describes the
distribution. Besides `cdf`, `density`, and `pmf`, other options
understood by `distionary` include:

- `survival`: the survival function, or one minus the cdf.

- `hazard`: the hazard function, for continuous variables only.

- `chf`: the cumulative hazard function, for continuous variables only.

- `quantile`: the quantile function, or left-inverse of the cdf.

- `realise` or `realize`: a function that takes an integer and generates
  a vector of that many random draws from the distribution.

- `odds`: for discrete variables, the probability odds function (pmf /
  (1 - pmf))

- `return`: the quantiles associated with the provided return periods,
  where events are exceedances.

All functions should be vectorized.

Other properties that are understood by `distionary` include:

- `mean`, `stdev`, `variance`, `skewness`, `median` are
  self-explanatory.

- `kurtosis_exc` and `kurtosis` are the distribution's excess kurtosis
  and regular kurtosis.

- `range`: A vector of the minimum and maximum value of a distribution's
  support.

## Examples

``` r
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
  .name = "My Linear",
  .parameters = list(could = "include", anything = data.frame(x = 1:10))
)

# Inspect
linear
#> My Linear distribution (continuous) 
#> --Parameters--
#> $could
#> [1] "include"
#> 
#> $anything
#>     x
#> 1   1
#> 2   2
#> 3   3
#> 4   4
#> 5   5
#> 6   6
#> 7   7
#> 8   8
#> 9   9
#> 10 10
#> 

# Plot
plot(linear)
```
