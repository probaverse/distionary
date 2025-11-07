# Plot a Distribution

Plot a distribution's representation.

## Usage

``` r
# S3 method for class 'dst'
plot(
  x,
  what = c("density", "pmf", "cdf", "survival", "quantile", "hazard", "chf"),
  ...
)
```

## Arguments

- x:

  Distribution object

- what:

  Name of the representation to plot.

- ...:

  Other arguments to pass to the
  [`graphics::curve`](https://rdrr.io/r/graphics/curve.html) function,
  or [`graphics::plot`](https://rdrr.io/r/graphics/plot.default.html) in
  the case of the PMF.

## Value

This function is run for its graphics byproduct, and therefore returns
the original distribution, invisibly.

## Examples

``` r
d <- dst_norm(0, 1)
plot(d, from = -4, to = 4)

plot(d, "cdf", n = 1000)

plot(d, "survival")

plot(d, "quantile")

plot(d, "hazard")

plot(d, "chf")


p <- dst_pois(4)
plot(p)
```
