# Log Pearson Type III distribution

Makes a Log Pearson Type III distribution, which is the distribution of
the exponential of a random variable following a Pearson Type III
distribution.

## Usage

``` r
dst_lp3(meanlog, sdlog, skew)
```

## Arguments

- meanlog:

  Mean of the log of the random variable; single numeric.

- sdlog:

  Standard deviation of the log of the random variable; single positive
  numeric.

- skew:

  Skewness of the log of the random variable; single numeric.

## Value

A Log Pearson Type III distribution.

## Examples

``` r
dst_lp3(0, 1, 1)
```
