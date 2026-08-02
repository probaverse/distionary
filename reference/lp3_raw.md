# Representations of the Log Pearson Type III Distribution

Representations of the Log Pearson Type III (LP3) Distribution

## Usage

``` r
plp3(x, meanlog, sdlog, skew, lower.tail = TRUE)

dlp3(x, meanlog, sdlog, skew)

qlp3(p, meanlog, sdlog, skew)

rlp3(n, meanlog, sdlog, skew)
```

## Arguments

- x:

  Vector of quantiles.

- meanlog:

  Parameter representing the mean of the random variable in log (base e)
  space; numeric. Vectors are allowed except for `rlp3()`.

- sdlog:

  Parameter representing the standard deviation of the random variable
  in log (base e) space; positive numeric. Vectors are allowed except
  for `rlp3()`.

- skew:

  Parameter representing the skewness of the random variable in log
  (base e) space; numeric. Vectors are allowed except for `rlp3()`.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are `P(X <= x)`,
  otherwise, `P(X > x)`.

- p:

  Vector of probabilities.

- n:

  Single positive whole number; number of observations to draw from the
  distribution.

## Value

Vector of evaluated LP3 distribution, with length equal to the recycled
lengths of `x`/`p`, `meanlog`, `sdlog`, and `skew`. For `rlp3()`, a
vector of length `n`.

## Examples

``` r
plp3(1:10, meanlog = 0, sdlog = 1, skew = 1)
#>  [1] 0.5665299 0.7850795 0.8655469 0.9055821 0.9289547 0.9440223 0.9544163
#>  [8] 0.9619487 0.9676161 0.9720082
dlp3(1:10, meanlog = 1:10, sdlog = 2, skew = 0)
#>  [1] 1.760327e-01 8.056299e-02 4.231526e-02 2.123059e-02 9.480554e-03
#>  [6] 3.633691e-03 1.169740e-03 3.117984e-04 6.813432e-05 1.211646e-05
qlp3(1:9 / 10, meanlog = 2, sdlog = 10, skew = 2)
#> [1] 9.620974e-04 3.124239e-03 1.187582e-02 5.547939e-02 3.435137e-01
#> [6] 3.199221e+00 5.681089e+01 3.276002e+03 3.354626e+06
set.seed(1)
rlp3(10, meanlog = 2, sdlog = 10, skew = 2)
#>  [1] 1.582758e-03 5.021233e+04 2.304318e+04 1.435878e+00 6.840435e+01
#>  [6] 3.600121e+01 6.687228e+00 7.253531e-03 8.641112e-04 1.615704e-03
```
