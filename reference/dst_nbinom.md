# Negative binomial Distribution

Makes a Negative Binomial distribution, corresponding to the number of
failures in a sequence of independent trials until a given number of
successes are observed.

## Usage

``` r
dst_nbinom(size, prob)
```

## Arguments

- size:

  Number of successful trials; single positive numeric.

- prob:

  Probability of a successful trial; single numeric between 0 and 1.

## Value

A Negative Binomial distribution.

## Examples

``` r
d <- dst_nbinom(10, 0.5)

# This version of the Negative Binomial distribution does not count
# the successes.
range(d)
#> [1]   0 Inf
```
