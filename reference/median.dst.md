# Median of a Distribution

Finds the median of a distribution.

## Usage

``` r
# S3 method for class 'dst'
median(x, ...)
```

## Arguments

- x:

  Distribution to calculate median from.

- ...:

  Not used.

## Value

Median of a distribution; single numeric.

## Details

Median is calculated as the 0.5-quantile when not found in the
distribution. So, when the median is non-unique, the smallest of the
possibilities is taken.

## Examples

``` r
d <- dst_gamma(3, 3)
median(d)
#> [1] 0.8913534
```
