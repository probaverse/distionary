# Finite Distribution

Makes a finite distribution, which is a distribution with a finite
number of possible outcomes.

## Usage

``` r
dst_finite(outcomes, probs)
```

## Arguments

- outcomes:

  Numeric vector representing the potential outcomes of the
  distribution.

- probs:

  Numeric vector of probabilities corresponding to the outcomes in
  `outcomes`. Must not be negative and must sum to 1.

## Value

A distribution with finite outcomes.

## See also

[`dst_empirical()`](https://distionary.probaverse.com/reference/dst_empirical.md)

## Examples

``` r
dst_finite(2:5, probs = 1:4 / 10)
#> Finite distribution (discrete) 
#> --Parameters--
#> # A tibble: 4 × 2
#>   outcomes probs
#>      <int> <dbl>
#> 1        2   0.1
#> 2        3   0.2
#> 3        4   0.3
#> 4        5   0.4
```
