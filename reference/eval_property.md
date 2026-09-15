# Evaluate a distribution

Evaluate a distribution property. The distribution itself is first
searched for the property, and if it can't be found, will attempt to
calculate the property from other entries.

## Usage

``` r
eval_property(distribution, entry, ...)
```

## Arguments

- distribution:

  Distribution object.

- entry:

  Name of the property, such as "cdf" or "mean". Length 1 character
  vector.

- ...:

  If the property is a function, arguments to the function go here. Need
  not be named; inserted in the order they appear.

## Value

The distribution's property, evaluated. If cannot be evaluated, returns
`NULL`.

## Examples

``` r
d <- distribution(
  cdf = function(x) {
    (x > 0) * pmin(x^2, 1)
  },
  g = 9.81,
  .vtype = "continuous"
)
#> Warning: Full suite of distribution properties may not be accessible without specifying 'cdf', and either 'density' or 'pmf'.
eval_property(d, "g")
#> [1] 9.81
eval_property(d, "quantile", 1:9 / 10)
#> [1] 0.3162278 0.4472136 0.5477226 0.6324555 0.7071068 0.7745967 0.8366600
#> [8] 0.8944272 0.9486833
eval_property(d, "mean")
#> Integration routine for numerical computation failed. This could be because the property is not well-defined. Returning NaN.
#> [1] NaN
eval_property(d, "realise", 10)
#>  [1] 0.2841657 0.9134183 0.7750877 0.3964952 0.0860200 0.6829301 0.7055334
#>  [8] 0.5383003 0.8560853 0.8789320
eval_property(d, "foofy")
#> NULL
eval_property(d, "foofy", 1:10)
#> NULL
```
