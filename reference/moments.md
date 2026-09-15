# Moments of a Distribution

Get common moment-related quantities of a distribution: `mean`,
`variance`, standard deviation (`stdev`), `skewness`, and `kurtosis` or
excess kurtosis (`kurtosis_exc`). If these quantities are not supplied
in the distribution's definition, a numerical algorithm may be used.

## Usage

``` r
kurtosis(distribution)

kurtosis_exc(distribution)

# S3 method for class 'dst'
mean(x, ...)

skewness(distribution)

stdev(distribution)

variance(distribution)
```

## Arguments

- x, distribution:

  Distribution to evaluate.

- ...:

  When calculating the mean via integration of the quantile function,
  arguments passed to
  [`stats::integrate()`](https://rdrr.io/r/stats/integrate.html).

## Value

A single numeric.

## Details

If a moment is not supplied in the distribution's definition, it is
computed numerically over the distribution's support: a sum over the
atoms (the discrete part) plus integration of the density over the
continuous part. An infinite atomic support (such as a Poisson's) is
summed by walking outward through its atoms until the tail contribution
is negligible.

## Note

When a moment is computed numerically and the underlying sum or integral
does not converge — for example, a heavy-tailed distribution whose
moment is not finite — the result is `NaN`.

## Examples

``` r
a <- dst_gp(1, 0.5)
b <- dst_unif(0, 1)
c <- dst_norm(3, 4)
mean(a)
#> [1] 2
variance(b)
#> [1] 0.08333333
kurtosis(c)
#> [1] 3
kurtosis_exc(c)
#> [1] 0
```
