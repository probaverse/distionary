# The Support of a Distribution

``` r

library(distionary)
```

This vignette explains what a distribution’s *support* is in
`distionary`, how to build one, and why it’s worth telling the package
about it.

## What a Support Is

A support says where a distribution’s probability lives, and in what
form.

Probability comes in two forms. **Mass** sits on single points.
**Density** is spread over regions. A support records both: the points
carrying mass, called **atoms**, and the **regions** carrying density.

That is two pieces of information, not one. Knowing which values are
possible is not enough — `[0, 1]` with a mass at 0 and `[0, 1]` without
one cover the same values but are different supports, and the
distributions over them differ.

The support says where the mass and density are. The distribution over
it says how much.

Which of the two a distribution has decides what it is called: discrete
if only mass, continuous if only density, mixed if both. So the variable
type is *derived* from the support rather than declared alongside it.

The reason to carry this around is practical. Numeric routines need to
know where the atoms are: a quantile has to land exactly on an atom
rather than near it, and a mean has to sum over atoms rather than
integrate across them. Recording the support means these can be done
correctly instead of guessed at.

## Building a Support

Three constructors build supports, one for each variable type.

### Continuous supports

[`continuous()`](https://distionary.probaverse.com/reference/support-construction.md)
takes intervals, each a length-2 numeric vector `c(lower, upper)`.

``` r

continuous(c(0, Inf))
#> <support: continuous>
#> -- continuous --
#> [0, Inf]
```

With no arguments at all, it defaults to the whole real line.

``` r

continuous()
#> <support: continuous>
#> -- continuous --
#> [-Inf, Inf]
```

Several intervals give a union, which is sorted and merged into a
canonical form. Notice that the overlapping pieces below collapse into
one:

``` r

continuous(c(3, 4), c(0, 1), c(0.5, 2))
#> <support: continuous>
#> -- continuous --
#> [0, 2] U [3, 4]
```

Intervals are written as closed, but their endpoints carry no
probability either way, a single point having measure zero. An interval
is really neither open nor closed. A point that genuinely carries mass
is an atom, and belongs in the atomic part instead. Each interval must
have `lower < upper`.

### Discrete supports

[`discrete()`](https://distionary.probaverse.com/reference/support-construction.md)
takes the atoms. Finitely many atoms can be given as a plain numeric
vector, which is sorted for you.

``` r

discrete(c(3.5, 1.2, 6.7))
#> <support: discrete>
#> -- atoms --
#> Numeric vector series of length 3:
#> 1.2, 3.5, 6.7
```

Distributions like the Poisson have infinitely many atoms, which can’t
be written out as a vector. These come from the `discretes` package,
whose series constructors `distionary` re-exports for convenience.

``` r

discrete(natural0())
#> <support: discrete>
#> -- atoms --
#> Integer series of length Inf:
#> 0, 1, 2, 3, 4, 5, ...
```

``` r

discrete(integers())
#> <support: discrete>
#> -- atoms --
#> Integer series of length Inf:
#> ..., -2, -1, 0, 1, 2, 3, ...
```

[`arithmetic()`](https://discretes.netlify.app/reference/arithmetic.html)
describes evenly spaced atoms, given a representative point and a
spacing. It runs forever in both directions unless you say otherwise:

``` r

discrete(arithmetic(0, 0.5))
#> <support: discrete>
#> -- atoms --
#> Arithmetic series of length Inf:
#> ..., -1, -0.5, 0, 0.5, 1, 1.5, ...
```

``` r

discrete(arithmetic(0, 0.5, n_left = 0, n_right = 4))
#> <support: discrete>
#> -- atoms --
#> Arithmetic series of length 5:
#> 0, 0.5, 1, 1.5, 2
```

### Mixed supports

[`mixed()`](https://distionary.probaverse.com/reference/support-construction.md)
takes both parts, named. Here is an atom at zero sitting at the edge of
a continuous tail:

``` r

mixed(discrete = 0, continuous = c(0, Inf))
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0
#> -- continuous --
#> [0, Inf]
```

An atom is allowed to coincide with a region’s endpoint, as it does here
– the atom is simply tracked separately, and its mass is not
double-counted against the density.

Because the variable type is derived from the parts,
[`mixed()`](https://distionary.probaverse.com/reference/support-construction.md)
has nothing to insist on. Hand it one half and you get a support of that
kind back, which makes it the one to reach for when the parts are worked
out by code rather than typed, and either might come out empty.

``` r

mixed(continuous = continuous(c(0, 1)))
#> <support: continuous>
#> -- continuous --
#> [0, 1]
```

[`discrete()`](https://distionary.probaverse.com/reference/support-construction.md)
and
[`continuous()`](https://distionary.probaverse.com/reference/support-construction.md)
remain the direct way to say one on its own.

### The empty support

There is a fourth constructor, for a support with nothing in it at all.

``` r

empty_support()
#> <support: empty>
```

No distribution has an empty support – probability has to go somewhere –
and
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
refuses one.

``` r

distribution(cdf = stats::pnorm, .support = empty_support())
#> Error in `distribution()`:
#> ! A distribution cannot have an empty support.
#> It has to place its probability somewhere; see `?empty_support`.
```

It exists so that operations on supports are *closed*. Restricting a
support to a region it doesn’t reach has to return something, and the
empty support is that something. Without it, every operation that can
come up empty would need a special case, and the thing it returned in
that case would have to mean something else.

Its variable type, shown when it prints, is `"empty"`, which says there
is nowhere to place probability at all. That is a different thing from a
distribution simply not saying where its probability lives, which
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md)
no longer permits.

``` r

is_empty_support(empty_support())
#> [1] TRUE
```

## Retrieving a Support

The built-in families all carry a support, which
[`support()`](https://distionary.probaverse.com/reference/support.md)
retrieves.

``` r

support(dst_pois(3))
#> <support: discrete>
#> -- atoms --
#> Integer series of length Inf:
#> 0, 1, 2, 3, 4, 5, ...
support(dst_gamma(shape = 2, rate = 1))
#> <support: continuous>
#> -- continuous --
#> [0, Inf]
```

The two parts can be pulled out individually.
[`atoms()`](https://distionary.probaverse.com/reference/atoms.md)
returns a `discretes` object, and
[`regions()`](https://distionary.probaverse.com/reference/atoms.md)
returns a two-column matrix of intervals.

``` r

atoms(mixed(discrete = 0, continuous = c(0, Inf)))
#> Numeric vector series of length 1:
#> 0
regions(continuous(c(0, 1), c(3, 4)))
#>      lower upper
#> [1,]     0     1
#> [2,]     3     4
```

Both accept a distribution as well as a support, so there’s no need to
unwrap first.

``` r

atoms(dst_binom(5, 0.3))
#> Numeric vector series of length 6:
#> 0, 1, 2, 3, 4, 5
```

The variable type and the range are both derived from the support, so
they always agree with it.

``` r

vtype(dst_pois(3))
#> [1] "discrete"
range(dst_pois(3))
#> [1]   0 Inf
```

A support has a range of its own, being the two outermost points it
reaches.

``` r

range(continuous(c(0, 1), c(3, 4)))
#> [1] 0 4
```

Gaps aren’t represented there. The support above places no probability
between 1 and 3, but its range spans that stretch all the same.

Finally,
[`is_support()`](https://distionary.probaverse.com/reference/is_support.md)
tests whether an object is a support.

``` r

is_support(continuous(c(0, 1)))
#> [1] TRUE
```

## Manipulating a Support

Supports can be combined, cut down, and mapped through functions. Every
one of these operations takes supports and gives back a support, so they
compose freely.

Two supports can be unioned. Overlapping or touching intervals are
merged, and the atoms pooled.

``` r

support_union(continuous(c(0, 1)), continuous(c(0.5, 3)))
#> <support: continuous>
#> -- continuous --
#> [0, 3]
```

An atom lying inside another support’s continuous part stays an atom.
The two carry different kinds of probability, and neither absorbs the
other.

``` r

support_union(discrete(3), continuous(c(0, 10)))
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 1:
#> 3
#> -- continuous --
#> [0, 10]
```

[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md)
cuts a support down to an interval. The `include_from` and `include_to`
arguments apply to the atoms only, since an interval endpoint carries no
probability either way.

``` r

support_restrict(discrete(natural0()), to = 4)
#> Loading required namespace: testthat
#> <support: discrete>
#> -- atoms --
#> Subset series of length 5:
#> 0, 1, 2, 3, 4
```

``` r

support_restrict(discrete(natural0()), to = 4, include_to = FALSE)
#> <support: discrete>
#> -- atoms --
#> Subset series of length 4:
#> 0, 1, 2, 3
```

Restricting to somewhere the support doesn’t reach is where the empty
support earns its keep.

``` r

support_restrict(continuous(c(0, 1)), from = 5, to = 6)
#> <support: empty>
```

A support can also be pushed through a strictly monotonic function.
Shifting and scaling are common enough to have their own forms.

``` r

support_shift(continuous(c(0, 1)), by = 5)
#> <support: continuous>
#> -- continuous --
#> [5, 6]
support_scale(discrete(natural0()), by = 2)
#> <support: discrete>
#> -- atoms --
#> Transformed series of length Inf:
#> 0, 2, 4, 6, 8, 10, ...
```

A decreasing map reverses each region, which is taken care of for you.

``` r

support_scale(continuous(c(1, 2)), by = -1)
#> <support: continuous>
#> -- continuous --
#> [-2, -1]
```

Scaling by zero is worth singling out, because it changes what kind of
support you have.

``` r

support_scale(continuous(c(1, 2)), by = 0)
#> <support: discrete>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0
```

Multiplying by zero sends every value to 0. Density that was spread over
a region is compressed onto that single point, and density compressed
onto a point is mass. So whatever the distribution was, the result has a
mass at 0 and density nowhere: it is discrete.

It only works in that direction. A mass sits on one point and lands on
one point, so mass stays mass.

A strictly monotonic map stretches and shifts regions but never squashes
one down to a point, so density stays density and mass stays mass. That
is why
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md)
asks for a monotonic map, and why scaling by zero — which is not one —
is handled separately.

Reciprocals are worth a closer look, because `1 / x` isn’t monotonic
across zero. Each side of zero is mapped separately and the two results
unioned, which is why the support below comes back in two pieces.

``` r

support_reciprocal(continuous(c(-2, 4)))
#> <support: continuous>
#> -- continuous --
#> [-Inf, -0.5] U [0.25, Inf]
```

For anything else,
[`support_transform()`](https://distionary.probaverse.com/reference/support_transform.md)
takes the map together with its inverse.

``` r

support_transform(
  continuous(c(0, Inf)),
  fun = exp, inv = log,
  domain = c(0, Inf), range = c(1, Inf)
)
#> <support: continuous>
#> -- continuous --
#> [1, Inf]
```

Atoms can be added and removed one at a time. This leaves the continuous
part alone, so removing an atom that sits on an interval leaves the
interval whole.

``` r

support_add_atoms(continuous(c(0, Inf)), 0)
#> <support: mixed>
#> -- atoms --
#> Numeric vector series of length 1:
#> 0
#> -- continuous --
#> [0, Inf]
```

Finally, two functions ask whether a value belongs to a support.
[`support_contains()`](https://distionary.probaverse.com/reference/support_contains.md)
counts both parts, whereas
[`support_has_atom()`](https://distionary.probaverse.com/reference/support_contains.md)
counts only the atoms — making it the one to reach for when what matters
is whether a point carries positive probability.

``` r

s <- mixed(discrete = 0, continuous = c(2, 5))
support_contains(s, at = c(0, 1, 3, 9))
#> [1]  TRUE FALSE  TRUE FALSE
support_has_atom(s, at = c(0, 1, 3, 9))
#> [1]  TRUE FALSE FALSE FALSE
```

## Giving a Distribution a Support

Supports are attached to user-defined distributions through the
`.support` argument of
[`distribution()`](https://distionary.probaverse.com/reference/distribution.md),
described in the [Specifying Your Own
Distribution](https://distionary.probaverse.com/articles/specify-user-defined.html)
vignette.

``` r

my_normal <- distribution(
  cdf = stats::pnorm,
  density = stats::dnorm,
  .support = continuous(c(-Inf, Inf))
)
vtype(my_normal)
#> [1] "continuous"
```

One rule is worth knowing about. A bare numeric vector is not accepted
as a support, because it is ambiguous: `c(0, Inf)` could just as easily
mean an interval from zero to infinity as it could mean two atoms. You
have to name which one you mean.

``` r

distribution(cdf = stats::pnorm, .support = c(0, Inf))
#> Error in `as_support()`:
#> ! A bare numeric vector is ambiguous as a support: `c(0, Inf)`
#> could be two atoms, or one interval.
#> Use `discrete()` for atoms, or `continuous()` for intervals.
```

Inside
[`discrete()`](https://distionary.probaverse.com/reference/support-construction.md)
and
[`mixed()`](https://distionary.probaverse.com/reference/support-construction.md)
the ambiguity is already resolved by the argument name, which is why a
bare vector is fine there.

A support is not optional. It is the one thing `distionary` asks you to
state rather than work out for you. In principle it could: a CDF holds
the answer, since its jumps are the atoms and it stops moving where the
distribution ends. But finding those numerically means hunting for
discontinuities in a function that can only be sampled, and the answer
would be an estimate — least reliable, as it happens, for the small
atoms and the long tails where being exact matters most. Rather than
guess well most of the time, `distionary` asks.

``` r

distribution(cdf = stats::pnorm, density = stats::dnorm)
#> Error in `distribution()`:
#> ! A distribution needs a support.
#> Pass `.support` a `continuous()`, `discrete()`, or `mixed()` set.
```

The older `.vtype` argument, which took a string such as `"continuous"`,
is defunct: it errors, pointing at `.support`. Naming a type was never
the same as saying where the probability is. `"discrete"` doesn’t say
which points carry mass, and `"continuous"` doesn’t say over what
region, so there is no way to translate one into the other — and
guessing would give wrong answers rather than an error.

## Why It Matters

The payoff shows up on distributions that have atoms. Consider a model
for daily rainfall: no rain at all with probability 0.3, and an
exponential amount otherwise. This is a genuinely mixed distribution,
with an atom at zero and a density on the positive half-line.

``` r

p0 <- 0.3
rate <- 1 / 5
rainfall <- distribution(
  cdf = function(x) {
    ifelse(x < 0, 0, p0 + (1 - p0) * stats::pexp(x, rate))
  },
  density = function(x) {
    ifelse(x <= 0, 0, (1 - p0) * stats::dexp(x, rate))
  },
  pmf = function(x) {
    ifelse(x == 0, p0, 0)
  },
  .support = mixed(discrete = 0, continuous = c(0, Inf)),
  .name = "Rainfall"
)
vtype(rainfall)
#> [1] "mixed"
```

Only the CDF, density, and PMF were specified. The quantile function is
derived, and because the support says there is an atom at zero, it lands
on zero exactly for every probability at or below 0.3 instead of
returning something merely close.

``` r

eval_quantile(rainfall, at = c(0.1, 0.3, 0.5, 0.9))
#> [1] 0.000000 0.000000 1.682361 9.729551
```

The mean is derived too, by summing over the atoms and integrating over
the intervals separately. Here the answer is `(1 - p0) / rate`, which is
3.5.

``` r

mean(rainfall)
#> [1] 3.5
variance(rainfall)
#> [1] 22.75
```

The same applies to a purely discrete distribution defined only by its
CDF and PMF. Quantiles come back as exact atoms:

``` r

my_poisson <- distribution(
  cdf = function(x) stats::ppois(x, 2.5),
  pmf = function(x) stats::dpois(x, 2.5),
  .support = discrete(natural0()),
  .name = "My Poisson"
)
eval_quantile(my_poisson, at = c(0.05, 0.3, 0.5, 0.9, 0.99))
#> [1] 0 2 2 5 7
```

Note that the support here has infinitely many atoms, and the mean is
still computed. The atoms are walked outward in batches, and the walk
stops only when two things are true at once: the probability still ahead
of it has been spent, and the atoms underfoot are adding next to nothing
to the running total.

The first of those is what makes it safe. How much probability lies
beyond a point is exactly what the CDF says, so the walk never stops
while there is mass waiting further out — not even if it happens to be
crossing a long stretch of atoms too small to notice.

``` r

mean(my_poisson)
#> [1] 2.5
```

## Current Scope

Two things are worth knowing about where supports stand today.

A support describes a distribution on the real line. Multivariate
supports would be composed from univariate ones, and the object already
reserves a place to record its dimension, but that composition isn’t
built yet.

There is no intersection. It hasn’t been needed —
[`support_restrict()`](https://distionary.probaverse.com/reference/support_restrict.md)
already covers cutting a support down to an interval, which is the case
that comes up in practice — so it waits until something calls for it.

And a note for anyone checking the theory. Strictly, a measure on the
real line splits into three parts, not two: mass on points, density over
regions, and a third kind with neither — all of its probability on a set
of zero total length, none of it sitting on any point. The Cantor
distribution is the usual example. This is the Lebesgue decomposition,
and the third part is called singular continuous. A support here has no
way to describe one, so such distributions are out of reach.
