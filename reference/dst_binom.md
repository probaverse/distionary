# Binomial Distribution

Makes a Binomial distribution, representing the number of successes in a
fixed number of independent trials.

## Usage

``` r
dst_binom(size, prob)
```

## Arguments

- size:

  Number of trials; single positive integer.

- prob:

  Success probability of each trial; single numeric between 0 and 1.

## Value

A binomial distribution.

## Examples

``` r
dst_binom(10, 0.6)
#> Binomial distribution (discrete) 
#> --Parameters--
#> size prob 
#> 10.0  0.6 
```
