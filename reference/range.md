# Range of Distribution

Range returns a vector of length two, with the minimum and maximum
values of the (support of the) distribution.

## Usage

``` r
# S3 method for class 'dst'
range(distribution, ...)
```

## Arguments

- distribution:

  Distribution to compute range from.

- ...:

  Not used; vestige of the
  [`base::range()`](https://rdrr.io/r/base/range.html) S3 generic.

## Value

Vector of length two, containing the minimum and maximum values of a
distribution.

## Details

If there are no methods for the distribution's class, the range is
calculated using
[`eval_quantile()`](https://distionary.probaverse.com/reference/quantile.md)
at 0 and at 1.

## Examples

``` r
a <- dst_gp(1, 0.5)
b <- dst_unif(0, 1)
c <- dst_norm(3, 4)
range(a)
#> [1]   0 Inf
range(b)
#> [1] 0 1
range(c)
#> [1] -Inf  Inf
```
