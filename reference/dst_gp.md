# Generalised Pareto Distribution

Makes a Generalized Pareto (GP) distribution, corresponding to the
limiting distribution of excesses over a threshold.

## Usage

``` r
dst_gp(scale, shape)
```

## Arguments

- scale:

  Scale parameter; single positive numeric.

- shape:

  Shape parameter; single positive numeric. This is also the extreme
  value index, so that `shape > 0` is heavy tailed, and `shape < 0` is
  short-tailed.

## Value

A Generalised Pareto Distribution.

## Examples

``` r
# Short-tailed example
short <- dst_gp(1, -1)
range(short)
#> [1] 0 1
mean(short)
#> [1] 0.5

# Heavy-tailed example
heavy <- dst_gp(1, 1)
range(heavy)
#> [1]   0 Inf
mean(heavy)
#> [1] Inf

# Light-tailed example (a Gumbel distribution)
light <- dst_gp(1, 0)
range(light)
#> [1]   0 Inf
mean(light)
#> [1] 1
```
