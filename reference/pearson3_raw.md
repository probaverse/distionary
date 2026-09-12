# Representations of the Pearson Type III Distribution

Representations of the Pearson Type III Distribution

## Usage

``` r
ppearson3(x, location, scale, shape, lower.tail = TRUE)

dpearson3(x, location, scale, shape, log = FALSE)

qpearson3(p, location, scale, shape)

rpearson3(n, location, scale, shape)
```

## Arguments

- x:

  Vector of quantiles.

- location:

  Parameter representing the boundary (endpoint) of the distribution;
  numeric. This is the left endpoint when `shape` is positive and the
  right endpoint when `shape` is negative. Vectors are allowed except
  for `rpearson3()`.

- scale:

  Scale parameter; positive numeric. Vectors are allowed except for
  `rpearson3()`.

- shape:

  Shape parameter; numeric. A negative value gives the distribution
  reflected about `location` (an upper-bounded, left-skewed
  distribution). Vectors are allowed except for `rpearson3()`.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are `P(X <= x)`,
  otherwise, `P(X > x)`.

- log:

  Logical; if `TRUE`, probabilities are given as log-probabilities.

- p:

  Vector of probabilities.

- n:

  Single positive whole number; number of observations to draw from the
  distribution.

## Value

Vector of evaluated Pearson Type III distribution, with length equal to
the recycled lengths of `x`/`p`, `location`, `scale`, and `shape`. For
`rpearson3()`, a vector of length `n`.

## Examples

``` r
ppearson3(1:10, location = 0, scale = 1, shape = 1)
#>  [1] 0.6321206 0.8646647 0.9502129 0.9816844 0.9932621 0.9975212 0.9990881
#>  [8] 0.9996645 0.9998766 0.9999546
dpearson3(1:10, location = 1:10, scale = 2, shape = 0)
#>  [1] Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf
qpearson3(1:9 / 10, location = -2, scale = 10, shape = 2)
#> [1]  3.318116  6.243883  8.973492 11.764213 14.783470 18.223132 22.392165
#> [8] 27.943083 36.897202
set.seed(1)
rpearson3(10, location = 2, scale = 10, shape = 2)
#>  [1] 10.308650 37.707563 36.631704 22.508119 40.854090 27.405402 24.880663
#>  [8] 13.492927  8.810215 10.356237
```
