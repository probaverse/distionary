# Degenerate Distribution

A degenerate distribution assigns a 100% probability to one outcome.

## Usage

``` r
dst_degenerate(location)
```

## Arguments

- location:

  Outcome of the distribution; single positive numeric.

## Value

A degenerate distribution

## Examples

``` r
d <- dst_degenerate(5)
realise(d)
#> [1] 5
variance(d)
#> [1] 0
```
