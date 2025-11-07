# Null Distribution

Sometimes it's convenient to work with a distribution object that is
akin to a missing value. This is especially true when programmatically
outputting distributions, such as when a distribution fails to fit to
data. This function makes such a distribution object. It always
evaluates to `NA`.

## Usage

``` r
dst_null()
```

## Value

A Null distribution.

## Examples

``` r
x <- dst_null()
mean(x)
#> [1] NA
eval_pmf(x, at = 1:10)
#>  [1] NA NA NA NA NA NA NA NA NA NA
```
