# Representations of the Generalized Pareto Distribution

Representations of the Generalized Pareto Distribution

## Usage

``` r
pgp(q, scale, shape, lower.tail = TRUE)

qgp(p, scale, shape)

dgp(x, scale, shape)
```

## Arguments

- scale:

  Vector of scale parameters; positive numeric.

- shape:

  Vector of shape parameters; positive numeric.

- lower.tail:

  Single logical. If `TRUE`, cdf (default); if `FALSE`, survival
  function.

- p:

  Vector of probabilities.

- x, q:

  Vector of quantiles.

## Value

Vector of evaluated GP distribution, with length equal to the recycled
lengths of `q`/`x`/`p`, `scale`, and `shape`.

## Examples

``` r
pgp(1:10, 1, 1)
#>  [1] 0.5000000 0.6666667 0.7500000 0.8000000 0.8333333 0.8571429 0.8750000
#>  [8] 0.8888889 0.9000000 0.9090909
dgp(1:10, 2, 0)
#>  [1] 0.303265330 0.183939721 0.111565080 0.067667642 0.041042499 0.024893534
#>  [7] 0.015098692 0.009157819 0.005554498 0.003368973
qgp(1:9 / 10, 10, -2)
#> [1] 0.95 1.80 2.55 3.20 3.75 4.20 4.55 4.80 4.95
```
