# Geometric Distribution

Makes a Geometric distribution, corresponding to the number of failures
in a sequence of independent trials before observing a success.

## Usage

``` r
dst_geom(prob)
```

## Arguments

- prob:

  Probability of success in each trial; single numeric between 0 and 1.

## Value

A Geometric distribution.

## Examples

``` r
d <- dst_geom(0.4)

# This version of the Geometric distribution does not count the success.
range(d)
#> [1]   0 Inf
```
