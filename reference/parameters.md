# Parameters of a Distribution

Get or set the parameters of a distribution, if applicable. See details.

## Usage

``` r
parameters(distribution)

parameters(distribution) <- value
```

## Arguments

- distribution:

  Distribution.

- value:

  A list of named parameter values, or `NULL`.

## Value

A list of the distribution parameters. More specifically, returns the
`"parameters"` entry of the list making up the probability distribution.

## Details

If a distribution is made by specifying parameter values (e.g., mean and
variance for a Normal distribution; shape parameters for a Beta
distribution), it is useful to keep track of what these parameters are.
This is done by adding `parameters` to the list of objects defining the
distribution; for instance,
`distribution(parameters = c(shape1 = 1.4, shape2 = 3.4))`. Note that no
checks are made to ensure the parameters are valid. It's important to
note that, in this version of distionary, manually changing the
parameters after the distribution has been created will not change the
functionality of the distribution, because the parameters are never
referred to when making calculations.

## Examples

``` r
a <- dst_beta(1, 2)
parameters(a)
#> $shape1
#> [1] 1
#> 
#> $shape2
#> [1] 2
#> 

b <- distribution(mean = 5)
#> Warning: Full suite of distribution properties may not be accessible without specifying 'cdf', and either 'density' or 'pmf'.
parameters(b)
#> list()
parameters(b) <- list(t = 7)
parameters(b)
#> $t
#> [1] 7
#> 
```
