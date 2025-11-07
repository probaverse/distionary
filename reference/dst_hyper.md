# Hypergeometric Distribution

Creates a Hypergeometric distribution. The parameterization used here is
the same as for
[`stats::phyper()`](https://rdrr.io/r/stats/Hypergeometric.html), where
the outcome can be thought of as the number of red balls drawn from an
urn of coloured balls, using a scoop that holds a fixed number of balls.

## Usage

``` r
dst_hyper(m, n, k)
```

## Arguments

- m:

  The number of red balls in the urn; single positive integer.

- n:

  The number of non-red balls in the urn; single positive integer.

- k:

  the number of balls drawn from the urn (between 0 and `m + n`); single
  positive integer.

## Value

A Hypergeometric distribution.

## Examples

``` r
dst_hyper(15, 50, 10)
#> Hypergeometric distribution (discrete) 
#> --Parameters--
#>  m  n  k 
#> 15 50 10 
```
