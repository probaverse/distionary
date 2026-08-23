#' Expectation of a function over a distribution's support
#'
#' Computes `E[g(X)]` by splitting the distribution into its atomic and
#' continuous parts (its Lebesgue decomposition, recorded in the support):
#'
#' \deqn{E[g(X)] = \sum_{x \in atoms} g(x) \, p(x)
#'                 + \sum_{intervals} \int g(x) \, f(x) \, dx,}
#'
#' where `p` is the pmf (atom masses) and `f` is the density of the continuous
#' part. The atomic sum is evaluated by `sum_over_atoms()` (which walks outward
#' and truncates once the tail is negligible, so infinite atomic supports such
#' as a Poisson's are handled); each interval of the continuous part is
#' integrated with `distionary_integrate()`.
#'
#' Every distribution declares a support, so there is no fallback: a
#' distribution without one is an error. The single exception, [dst_null()],
#' supplies its own moments and never reaches here.
#'
#' @param distribution Distribution object.
#' @param g Function; the quantity whose expectation is taken (vectorised).
#' @param tol Tolerance passed to the atomic-sum truncation and the integrator.
#' @param ... Further arguments passed to `stats::integrate()` via
#' `distionary_integrate()`.
#' @returns A single numeric, or `NaN` if the expectation does not converge or
#' is not well-defined.
#' @noRd
expect_over_support <- function(distribution, g, tol = 1e-9, ...) {
  checkmate::assert_class(distribution, "dst")
  support <- attributes(distribution)[["support"]]
  if (is.null(support)) {
    # Unreachable in practice: `distribution()` requires a support, and the one
    # distribution without one (Null) supplies its own moments.
    stop(
      "Numerical computation requires the distribution's support. ",
      "Specify `.support` when building the distribution."
    )
  }
  total <- 0
  atoms <- support[["atoms"]]
  if (discretes::num_discretes(atoms) > 0) {
    pmf <- representation_as_function(distribution, "pmf")
    cdf <- representation_as_function(distribution, "cdf")
    total <- total + sum_over_atoms(atoms, pmf, cdf, g, tol = tol)
  }
  intervals <- support[["continuous"]]
  if (nrow(intervals) > 0) {
    dens <- representation_as_function(distribution, "density")
    # In a mixed distribution the density has a cusp at every atom, so the
    # integrand is non-smooth; give the adaptive integrator extra subdivisions.
    # (Left out for purely continuous distributions, whose tuned handling of
    # heavy tails should not be perturbed.)
    extra <- if (discretes::num_discretes(atoms) > 0) {
      list(subdivisions = 1000L)
    } else {
      list()
    }
    for (i in seq_len(nrow(intervals))) {
      total <- total + do.call(
        distionary_integrate,
        c(
          list(
            function(x) g(x) * dens(x),
            lower = intervals[i, "lower"], upper = intervals[i, "upper"],
            tol = tol
          ),
          extra,
          list(...)
        )
      )
    }
  }
  total
}

#' Sum `g(x) p(x)` over the atoms of a discretes series
#'
#' A finite series is enumerated and summed directly. An infinite series is
#' summed by walking it with batched `discretes::next_discrete()` /
#' `discretes::prev_discrete()` calls, accumulating incrementally and
#' truncating a tail once its terms are negligible.
#'
#' Walks cannot pass an accumulation point (a sink), so the line is
#' partitioned at the finite sinks and each segment is walked outward from an
#' interior anchor atom; the segment sums, plus any atoms sitting exactly on a
#' sink, are added together. With no finite sinks --- Poisson, geometric, the
#' integers --- there is a single segment, walked outward from a
#' representative atom.
#'
#' @param series A `discretes` object (the atomic support).
#' @param pmf Probability mass function (vectorised).
#' @param g Function whose expectation is being accumulated (vectorised).
#' @param tol Tolerance for the tail-truncation test.
#' @param max_atoms Cap on atoms visited per walk direction; see
#' `walk_atoms()` for what happens when it is exceeded.
#' @param batch Number of atoms stepped per `next_discrete()` /
#' `prev_discrete()` call; `g` and `pmf` are evaluated vectorised over each
#' batch.
#' @returns A single numeric, or `NaN` if the sum does not converge.
#' @noRd
sum_over_atoms <- function(
  series, pmf, cdf, g, tol = 1e-9, max_atoms = 1e5L, batch = 100L
) {
  n <- discretes::num_discretes(series)
  if (n == 0) {
    return(0)
  }
  if (is.finite(n)) {
    xs <- as.double(series)
    return(sum(g(xs) * pmf(xs)))
  }
  finite_sinks <- finite_sink_locations(series)
  breaks <- c(-Inf, finite_sinks, Inf)
  total <- 0
  for (i in seq_len(length(breaks) - 1L)) {
    total <- total + sum_atoms_between(
      series, pmf, cdf, g, breaks[i], breaks[i + 1L], tol, max_atoms, batch
    )
  }
  if (length(finite_sinks) > 0) {
    on_sink <- finite_sinks[discretes::has_discretes(series, finite_sinks)]
    if (length(on_sink) > 0) {
      total <- total + sum(g(on_sink) * pmf(on_sink))
    }
  }
  total
}

#' Finite (interior) accumulation points of a series, sorted and unique.
#' @noRd
finite_sink_locations <- function(series) {
  sk <- discretes::sinks(series)
  if (is.null(sk) || nrow(sk) == 0) {
    return(numeric(0))
  }
  loc <- sk[, "location"]
  sort(unique(loc[is.finite(loc)]))
}

#' Sum `g(x) p(x)` over the atoms strictly between consecutive sinks `a` and
#' `b` (each a finite accumulation point or +/-Inf), by walking outward in
#' both directions from an anchor atom inside the segment. Atoms exactly at
#' `a` or `b` are excluded; the caller accounts for atoms sitting on a sink.
#' @noRd
sum_atoms_between <- function(
  series, pmf, cdf, g, a, b, tol, max_atoms, batch
) {
  anchor <- find_anchor(series, a, b)
  if (is.null(anchor)) {
    return(0) # No atoms in this segment.
  }
  # How much probability this segment holds beyond a point, in each direction.
  # Bounded to the segment rather than to the whole line: a segment ending at
  # an accumulation point is never stepped past, so a walk inside it has to be
  # able to go quiet on its own, and the mass sitting in other segments must
  # not hold it open.
  f_a <- if (is.infinite(a)) 0 else cdf(a)
  f_b <- if (is.infinite(b)) 1 else cdf(b) - pmf(b)
  above <- function(x) max(0, f_b - cdf(x))
  below <- function(x) max(0, (cdf(x) - pmf(x)) - f_a)
  g(anchor) * pmf(anchor) +
    walk_atoms(
      series, anchor, discretes::next_discrete, pmf, g,
      bound = b, upward = TRUE, remaining = above, tol = tol,
      max_atoms = max_atoms, batch = batch
    ) +
    walk_atoms(
      series, anchor, discretes::prev_discrete, pmf, g,
      bound = a, upward = FALSE, remaining = below, tol = tol,
      max_atoms = max_atoms, batch = batch
    )
}

#' Find an atom of `series` strictly inside `(a, b)`, or `NULL` if none.
#'
#' Probes outward from an interior point `m`: the smallest atom at or above
#' `m`, then the largest atom below `m`. Between them the two probes see every
#' atom in the segment, so if both fail the segment is empty.
#' @noRd
find_anchor <- function(series, a, b) {
  if (is.infinite(a) && is.infinite(b)) {
    return(discretes::representative(series))
  }
  m <- if (is.finite(a) && is.finite(b)) {
    (a + b) / 2
  } else if (is.finite(a)) {
    a + 1
  } else {
    b - 1
  }
  up <- discretes::next_discrete(series, m, include_from = TRUE)
  if (length(up) == 1 && !is.na(up) && up > a && up < b) {
    return(up)
  }
  down <- discretes::prev_discrete(series, m)
  if (length(down) == 1 && !is.na(down) && down > a && down < b) {
    return(down)
  }
  NULL
}

#' Walk the atoms of a series in one direction, accumulating `g(x) p(x)`
#'
#' Atoms are visited in batches via `step_fn` (`discretes::next_discrete()` or
#' `prev_discrete()`), with `g` and `pmf` evaluated vectorised over each batch
#' and the sum accumulated incrementally. The walk stops when:
#'
#' - the series ends in this direction (a step returns no atoms), or the walk
#'   meets `bound` (a neighbouring sink --- atoms beyond it belong to the next
#'   segment and are excluded);
#' - a whole batch is negligible: every mass `p(x) < tol` *and* every term
#'   `|g(x) p(x)| < tol`. Requiring small mass avoids stopping where `g`
#'   happens to vanish; requiring a small term avoids stopping where `g` grows
#'   fast enough to offset a small mass;
#' - `max_atoms` atoms have been visited without the tail going quiet, in which
#'   case the sum is deemed not to converge and `NaN` is returned.
#' @noRd
walk_atoms <- function(
  series, from, step_fn, pmf, g, bound, upward, remaining, tol, max_atoms,
  batch
) {
  acc <- 0
  visited <- 0L
  x <- from
  repeat {
    xs <- step_fn(series, x, n = batch)
    xs <- xs[is.finite(xs)]
    if (length(xs) == 0L) {
      return(acc) # Series ends in this direction.
    }
    inside <- if (upward) xs < bound else xs > bound
    met_bound <- !all(inside)
    xs <- xs[inside]
    if (length(xs) > 0L) {
      p <- pmf(xs)
      terms <- g(xs) * p
      acc <- acc + sum(terms)
      visited <- visited + length(xs)
      frontier <- if (upward) max(xs) else min(xs)
      # Two different things have to be true to stop, and neither implies the
      # other. That the atoms underfoot are small says the walk is not in the
      # middle of the mass; that the probability ahead is spent says there is
      # no more of it waiting further out. Without the second, a distribution
      # that thins to nothing and then resumes is cut off in the gap.
      quiet <- isTRUE(all(p < tol)) &&
        isTRUE(all(abs(terms) < tol)) &&
        isTRUE(remaining(frontier) < tol)
      if (quiet) {
        return(acc)
      }
    }
    if (met_bound) {
      return(acc)
    }
    if (visited >= max_atoms) {
      return(NaN)
    }
    x <- if (upward) max(xs) else min(xs)
  }
}
