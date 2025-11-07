# Distribution name

Print the name of a distribution, possibly with parameters.

## Usage

``` r
pretty_name(distribution, param_digits = 0)
```

## Arguments

- distribution:

  Distribution object.

- param_digits:

  How many significant digits to include when displaying the parameters?
  `0` if you don't want to display parameters. Length 1 vector.

## Value

A character containing the distribution's name, possibly followed by
parameters in brackets.

## Examples

``` r
d <- dst_norm(0.3552, 1.1453)
pretty_name(d)
#> [1] "Normal"
pretty_name(d, 2)
#> [1] "Normal(0.36, 1.1)"
```
