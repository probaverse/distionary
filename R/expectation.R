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
#' If the distribution has no structured support (a legacy distribution defined
#' with a `.vtype` string), this falls back to the previous behaviour:
#' integrating the density over the distribution's range for continuous
#' distributions, and erroring otherwise.
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
    # Legacy fallback: no structured support to decompose.
    if (identical(vtype(distribution), "continuous")) {
      dens <- representation_as_function(distribution, "density")
      r <- range(distribution)
      return(distionary_integrate(
        function(x) g(x) * dens(x),
        lower = r[1], upper = r[2], tol = tol, ...
      ))
    }
    stop(
      "Numerical computation for non-continuous distributions requires a ",
      "structured support. Specify `.support` in the distribution."
    )
  }
  total <- 0
  atoms <- support[["atoms"]]
  if (discretes::num_discretes(atoms) > 0) {
    pmf <- representation_as_function(distribution, "pmf")
    total <- total + sum_over_atoms(atoms, pmf, g, tol = tol)
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
#' handled according to its accumulation points (sinks):
#'
#' - With sinks only at +/-Inf (the usual case --- Poisson, geometric, the
#'   integers), the sum is built by walking outward from a representative atom
#'   via `discretes::next_discrete()` / `prev_discrete()`, truncating once the
#'   tail is negligible (see `walk_sum()`).
#' - With a *finite* (interior) sink --- a point the atoms accumulate towards,
#'   such as `1/n -> 0` --- a spatial walk would stall at the sink and never
#'   reach atoms on the far side. Instead the line is partitioned at the finite
#'   sinks and each segment summed by `sum_segment()`, which queries
#'   `discretes::get_discretes_in()` over windows that grow towards (but never
#'   reach) each sink.
#'
#' @param series A `discretes` object (the atomic support).
#' @param pmf Probability mass function (vectorised).
#' @param g Function whose expectation is being accumulated (vectorised).
#' @param tol Tolerance for the tail-truncation test.
#' @param max_iter Cap on steps; exceeding it returns `NaN` (non-convergence,
#' e.g. a divergent moment).
#' @param patience Number of consecutive negligible steps required to stop.
#' @returns A single numeric, or `NaN` if the sum did not converge.
#' @noRd
sum_over_atoms <- function(series, pmf, g, tol = 1e-9, max_iter = 1e5L,
                           patience = 5L) {
  n <- discretes::num_discretes(series)
  if (n == 0) {
    return(0)
  }
  if (is.finite(n)) {
    xs <- as.double(series)
    return(sum(g(xs) * pmf(xs)))
  }
  finite_sinks <- finite_sink_locations(series)
  if (length(finite_sinks) == 0) {
    return(walk_sum(series, pmf, g, tol, max_iter, patience))
  }
  # Partition the line at the finite sinks; sum each open segment, plus any
  # atoms that happen to sit exactly on a sink location.
  breaks <- c(-Inf, finite_sinks, Inf)
  total <- 0
  for (i in seq_len(length(breaks) - 1L)) {
    total <- total + sum_segment(
      series, pmf, g, breaks[i], breaks[i + 1L], tol, max_iter, patience
    )
  }
  on_sink <- finite_sinks[discretes::has_discretes(series, finite_sinks)]
  if (length(on_sink) > 0) {
    total <- total + sum(g(on_sink) * pmf(on_sink))
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

#' Sum `g(x) p(x)` over the atoms in an open segment `(a, b)` whose only
#' accumulation points are its endpoints (each a finite sink or +/-Inf).
#'
#' A window `[l, r]` strictly inside `(a, b)` is grown towards each endpoint
#' (geometrically towards a finite sink, by doubling towards an infinite one).
#' Because no sink lies strictly inside the window, `get_discretes_in()` returns
#' a finite atom set, which is summed. The window never reaches a sink, so the
#' sum converges from below; it stops once newly admitted atoms contribute less
#' than `tol` for `patience` consecutive steps.
#' @noRd
sum_segment <- function(series, pmf, g, a, b, tol, max_iter, patience) {
  anchor <- if (is.finite(a) && is.finite(b)) {
    (a + b) / 2
  } else if (is.finite(a)) {
    a + 1
  } else if (is.finite(b)) {
    b - 1
  } else {
    0
  }
  prev_sum <- 0
  small <- 0L
  for (k in seq_len(max_iter)) {
    l <- if (is.finite(a)) a + (anchor - a) * 2^(-k) else anchor - 2^k
    r <- if (is.finite(b)) b - (b - anchor) * 2^(-k) else anchor + 2^k
    # Underflow towards a finite sink: the window can grow no further.
    if ((is.finite(a) && l <= a) || (is.finite(b) && r >= b)) {
      return(prev_sum)
    }
    # `get_discretes_in()` can error on some derived series (e.g. a union whose
    # window falls outside one component's range). Rather than crash or silently
    # truncate, report the moment as not computable.
    xs <- tryCatch(
      discretes::get_discretes_in(series, from = l, to = r),
      error = function(e) NULL
    )
    if (is.null(xs)) {
      return(NaN)
    }
    this_sum <- if (length(xs) == 0L) 0 else sum(g(xs) * pmf(xs))
    delta <- this_sum - prev_sum
    if (is.finite(delta) && abs(delta) < tol) {
      small <- small + 1L
      if (small >= patience) {
        return(this_sum)
      }
    } else {
      small <- 0L
    }
    prev_sum <- this_sum
  }
  NaN
}

#' Sum over an infinite series whose only sinks are at +/-Inf, by walking
#' outward from a representative atom in each direction.
#'
#' A term contributes negligibly when both its mass `p(x)` and its contribution
#' `g(x) p(x)` fall below `tol` for `patience` consecutive atoms. Requiring the
#' mass to be small avoids stopping early at an interior atom where `g` happens
#' to vanish; requiring the contribution to be small avoids stopping early when
#' `g` grows fast enough to offset a small mass.
#' @noRd
walk_sum <- function(series, pmf, g, tol, max_iter, patience) {
  start <- discretes::representative(series)
  total <- g(start) * pmf(start)
  up <- walk_atoms(series, start, discretes::next_discrete, pmf, g,
                   tol, max_iter, patience)
  down <- walk_atoms(series, start, discretes::prev_discrete, pmf, g,
                     tol, max_iter, patience)
  total + up + down
}

#' Walk the atoms in one direction, accumulating `g(x) p(x)`
#' @returns Accumulated contribution; `0` if the direction ends immediately at a
#' finite boundary; `NaN` if it did not converge within `max_iter`.
#' @noRd
walk_atoms <- function(series, from, step_fn, pmf, g, tol, max_iter, patience) {
  x <- from
  acc <- 0
  small <- 0L
  for (i in seq_len(max_iter)) {
    x <- step_fn(series, x)
    if (length(x) == 0L || is.na(x) || is.infinite(x)) {
      return(acc) # Reached a finite end of the series in this direction.
    }
    p <- pmf(x)
    term <- g(x) * p
    acc <- acc + term
    negligible <- is.finite(p) && p < tol &&
      is.finite(term) && abs(term) < tol
    if (negligible) {
      small <- small + 1L
      if (small >= patience) {
        return(acc)
      }
    } else {
      small <- 0L
    }
  }
  NaN
}
