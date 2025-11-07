# Cauchy Distribution

Makes a Cauchy distribution.

## Usage

``` r
dst_cauchy(location, scale)
```

## Arguments

- location:

  Location parameter; single numeric.

- scale:

  Scale parameter; single positive numeric.

## Value

A Cauchy distribution.

## Examples

``` r
d <- dst_cauchy(0, 1)

# Moments do not exist for the Cauchy distribution.
mean(d)
#> [1] NaN
variance(d)
#> [1] NaN
```
