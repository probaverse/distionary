# Representations of the Generalized Extreme Value Distribution

Representations of the Generalized Extreme Value Distribution

## Usage

``` r
pgev(q, location, scale, shape)

qgev(p, location, scale, shape)

dgev(x, location, scale, shape)
```

## Arguments

- location:

  Location parameter; numeric vector.

- scale:

  Scale parameter; positive numeric vector.

- shape:

  Shape parameter; numeric vector. This is also the extreme value index,
  so that `shape > 0` is heavy tailed, and `shape < 0` is short-tailed.

- p:

  Vector of probabilities.

- x, q:

  Vector of quantiles.

## Value

Vector of evaluated GEV distribution, with length equal to the recycled
lengths of `q`/`x`/`p`, `location`, `scale`, and `shape`.

## Examples

``` r
pgev(1:10, 0, 1, 1)
#>  [1] 0.6065307 0.7165313 0.7788008 0.8187308 0.8464817 0.8668779 0.8824969
#>  [8] 0.8948393 0.9048374 0.9131007
dgev(1:10, 1:10, 2, 0)
#>  [1] 0.1839397 0.1839397 0.1839397 0.1839397 0.1839397 0.1839397 0.1839397
#>  [8] 0.1839397 0.1839397 0.1839397
qgev(1:9 / 10, 2, 10, -2)
#> [1] -19.5094906  -5.9514520  -0.2477526   2.8020565   4.5977349   5.6952859
#> [7]   6.3639149   6.7510348   6.9444958
```
