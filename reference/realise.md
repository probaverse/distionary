# Generate a Sample from a Distribution

Draw `n` independent observations from a distribution.

## Usage

``` r
realise(distribution, n = 1)

realize(distribution, n = 1)
```

## Arguments

- distribution:

  Distribution object.

- n:

  Number of observations to generate.

## Value

Vector of independent values drawn from the inputted distribution.

## Note

`realise()` and `realize()` are aliases and do the same thing.

## Examples

``` r
d <- dst_pois(5)
set.seed(2)
realise(d, n = 10)
#>  [1] 3 6 5 3 9 9 3 7 5 5
```
