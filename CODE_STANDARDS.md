# Code standards

Conventions for this package, part of the
[probaverse](https://probaverse.com). This is a living document; add to it as
conventions are settled.

## Formatting

- Format R code with [air](https://posit-dev.github.io/air/).
- Lines are at most **80** characters.
- Code chunks in `.Rmd`/`.qmd` files, and `@examples` in `.Rd` documentation,
  are at most **72** characters. These are indented or boxed when rendered, so
  the tighter limit keeps them from overflowing.

### Breaking a call across lines

When a call or a function signature does not fit on one line, break it so that
each argument sits on its own line, indented one level, with the closing
bracket on a line of its own. Do **not** indent continuation lines to line up
with an opening bracket on a previous line.

``` r
# Yes
vtype(distribution(
  cdf = pnorm,
  density = dnorm,
  .support = continuous()
))

support_restrict <- function(
  support,
  ...,
  from = -Inf,
  to = Inf
) {

# No
vtype(distribution(cdf = pnorm, density = dnorm,
                   .support = continuous()))

support_restrict <- function(support,
                             ...,
                             from = -Inf,
                             to = Inf) {
```

Alignment ties the indentation of every argument to the length of whatever
precedes the bracket, so renaming the function reindents the whole call and
produces diff noise unrelated to the change. It also eats horizontal space,
which bites hardest against the 72-character limit in examples.

[air](https://posit-dev.github.io/air/) formats R code this way already, but it
does not reach inside roxygen `@examples`, so those have to be written this way
by hand.

## Function arguments

- Match an argument against a set of allowed string values with
  `rlang::arg_match()`, not base R's `match.arg()`.
- In exported (public-facing) functions, lean towards placing `...` between the
  mandatory arguments and the optional ones (those with defaults), unless that
  is awkward. This forces optional arguments to be named, which keeps call
  sites readable and lets the argument list grow without breaking positional
  calls.
- When `...` is present but not consumed, call `rlang::check_dots_empty()` so
  that misspelled or misplaced arguments error rather than being silently
  ignored. (Omit this only when `...` is genuinely used.)
