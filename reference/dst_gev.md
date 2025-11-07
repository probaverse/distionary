# Generalised Extreme Value Distribution

Makes a Generalised Extreme Value (GEV) distribution, which is the
limiting distribution of the maximum.

## Usage

``` r
dst_gev(location, scale, shape)
```

## Arguments

- location:

  Location parameter; single numeric.

- scale:

  Scale parameter; single positive numeric.

- shape:

  Shape parameter; single numeric. This is also the extreme value index,
  so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.

## Value

A GEV distribution.

## Examples

``` r
# Short-tailed example
short <- dst_gev(0, 1, -1)
range(short)
#> [1] -Inf    1
mean(short)
#> [1] 0

# Heavy-tailed example
heavy <- dst_gev(0, 1, 1)
range(heavy)
#> [1]  -1 Inf
mean(heavy)
#> [1] Inf

# Light-tailed example (a Gumbel distribution)
light <- dst_gev(0, 1, 0)
range(light)
#> [1] -Inf  Inf
mean(light)
#> [1] 0.5772157
```
