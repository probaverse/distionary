# Aggregate discrete values

Aggregates discrete values together with their weights into a data frame
or tibble.

## Usage

``` r
aggregate_weights(y, weights, sum_to_one = FALSE)
```

## Arguments

- y:

  Vector of outcomes.

- weights:

  Vector of weights, one for each of `y`. These need not sum to one, but
  must not be negative and non-NA.

- sum_to_one:

  Logical; should the weights be normalized to sum to 1? Default is
  FALSE.

## Value

Data frame with the following columns:

- `y`: Increasing vector of unique values of `y` that have positive
  weight.

- `weight`: Weights corresponding to each outcome.

## Details

For a vector of outcomes `y` with a matching vector of `weights`,
`aggregate_weights()` provides a single non-zero, non-NA weight per
unique value of `y`.
