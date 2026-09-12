# Specify the Support of a Distribution

A support says where a distribution's probability lives, and in what
form. Probability comes in two forms: *mass*, which sits on single
points, and *density*, which is spread over regions. A support records
both — the points carrying mass, its *atoms*, and the *regions* carrying
density — and `discrete()`, `continuous()` and `mixed()` build one from
those pieces.

## Usage

``` r
discrete(atoms = numeric(0))

continuous(...)

mixed(discrete = numeric(0), continuous = numeric(0))
```

## Arguments

- atoms:

  For `discrete()`, the points carrying mass: a `discretes` object (see
  the discretes package, e.g.
  [`discretes::natural0()`](https://discretes.netlify.app/reference/integers.html)),
  a numeric vector of finitely many atoms, or a purely discrete support.
  A bare numeric vector is unambiguous here because the argument names
  the intent (contrast with passing one to `.support`, which is
  rejected).

- ...:

  For `continuous()`, one or more regions, each given as a length-2
  numeric `c(lower, upper)`. With no arguments, `continuous()` defaults
  to the whole real line, `c(-Inf, Inf)`. Overlapping or touching
  regions are merged and sorted into a canonical form.

- discrete, continuous:

  For `mixed()`, the two halves. Each takes the same things its own
  constructor takes, or a support already built by it: `discrete` as for
  `atoms` above, `continuous` as for `...` below.

## Value

A support object (class `"support"`).

## Details

The variable type
([`vtype()`](https://distionary.probaverse.com/reference/vtype.md)) is
*derived* from the support: a support with only atoms is `"discrete"`,
only a continuous part is `"continuous"`, both is `"mixed"`, and neither
is `"empty"`.

Because the type is derived, none of the three insists on being handed
something non-empty. Each builds whatever the parts describe, and
describing nothing gives
[`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md).
So `mixed(continuous = continuous())` is the whole real line,
`discrete(numeric(0))` is empty, and `mixed()` is empty too. This is
what makes them usable when the parts are computed rather than typed and
may come out empty; `mixed()` is then the general constructor, with
`discrete()` and `continuous()` the direct way to say one kind on its
own.

A region is written as a closed interval, but its endpoints carry no
probability either way, a single point having no width, so open against
closed makes no difference there. An atom that happens to sit on a
region's boundary is simply tracked as an atom.

Recording where the mass is and where the density is are two pieces of
information, not one. Knowing which values are possible is not enough:
`continuous(c(0, 1))` and `mixed(discrete = 0, continuous = c(0, 1))`
cover the same values, but they are different supports and the
distributions over them differ: one has `P(X = 0) = 0`, the other does
not. This is why an atom lying inside a region is kept rather than
absorbed into it.

### The third kind

Strictly, a measure on the real line splits into three parts, not two:
mass on points, density over regions, and a third kind with neither —
all of its probability on a set of zero total length, none of it sitting
on any point. The Cantor distribution is the usual example. This is the
Lebesgue decomposition, and the third part is called singular
continuous. A support here has no way to describe one, so such
distributions are out of reach.

## See also

[`support()`](https://distionary.probaverse.com/reference/support.md) to
retrieve a distribution's support,
[`vtype()`](https://distionary.probaverse.com/reference/vtype.md) for
the derived variable type.

Other Support:
[`atoms()`](https://distionary.probaverse.com/reference/atoms.md),
[`empty_support()`](https://distionary.probaverse.com/reference/empty_support.md),
[`is_support()`](https://distionary.probaverse.com/reference/is_support.md),
[`support()`](https://distionary.probaverse.com/reference/support.md)

## Examples

``` r
discrete(discretes::natural0())   # e.g. the support of a Poisson
#> <support: discrete>
#> -- atoms --
#> Integer series of length Inf:
#> 0, 1, 2, 3, 4, 5, ...
discrete(c(3.5, 1.2, 6.7))        # finitely many atoms
#> <support: discrete>
#> -- atoms --
#> Numeric vector series of length 3:
#> 1.2, 3.5, 6.7
continuous(c(0, Inf))             # e.g. the support of a Gamma
#> <support: continuous>
#> -- continuous --
#> [0, Inf]
continuous(c(0, 1), c(3, 4))      # a union of regions
#> <support: continuous>
#> -- continuous --
#> [0, 1] U [3, 4]
mixed(discrete = 0, continuous = c(0, Inf))  # an atom, plus a tail
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0
#> -- continuous --
#> [0, Inf]
```
