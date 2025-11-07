# Find the probability left or right of a number

Probability to the left or right of a number, inclusive or not.
`prob_left()` is a more general cdf defined using either `<` or `<=`,
and `prob_right()` is a more general survival function defined using
either `>` or `>=`.

## Usage

``` r
prob_left(distribution, of, inclusive)

prob_right(distribution, of, inclusive)
```

## Arguments

- distribution:

  Distribution to find probabilities of.

- of:

  Find the probability to the left or right *of* this number. Could be a
  vector.

- inclusive:

  Should `of` be included in the probability calculation? Logical.

## Value

A vector of probabilities.

## Examples

``` r
d <- dst_pois(5)
prob_left(d, of = 3, inclusive = TRUE)
#> [1] 0.2650259
prob_left(d, of = 3, inclusive = FALSE)
#> [1] 0.124652
prob_right(d, of = 0:3, inclusive = TRUE)
#> [1] 1.0000000 0.9932621 0.9595723 0.8753480
```
