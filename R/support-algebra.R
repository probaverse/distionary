# Support algebra -------------------------------------------------------------
#
# Operations on supports alone: supports in, supports out. No distribution is
# involved, which is what separates these from the support *inference* that
# belongs with the verbs that manipulate distributions (whether the boundary of
# `max(X, Y)` carries mass depends on the distributions, not just their
# supports).
#
# Every operation is closed: an operation that removes everything returns
# `empty_support()` rather than a sentinel.

#' Combine Supports
#'
#' The union of two or more supports: everything covered by any of them.
#'
#' @param ... Supports to combine, or a single list of them. Distributions are
#' accepted in place of supports. With no arguments, the result is
#' [empty_support()], which is the identity for this operation.
#' @details
#' The atomic parts are unioned as series, and the continuous parts are pooled
#' and merged back into canonical form, so touching or overlapping intervals
#' come out as one.
#'
#' An atom that falls inside another support's continuous part stays an atom.
#' The two carry different kinds of probability, and one does not absorb the
#' other.
#' @returns A support object.
#' @seealso [support_restrict()] to cut a support down instead.
#' @examples
#' support_union(continuous(c(0, 1)), continuous(c(0.5, 3)))
#' support_union(discrete(c(1, 2)), continuous(c(5, 6)))
#'
#' # The identity.
#' support_union()
#' @family Support algebra
#' @export
support_union <- function(...) {
  dots <- list(...)
  # A single bare list of supports: unwrap it, matching `continuous()`. Both
  # supports and distributions are themselves lists, so classed objects are
  # left alone.
  if (length(dots) == 1L && is.list(dots[[1L]]) && !is.object(dots[[1L]])) {
    dots <- dots[[1L]]
  }
  if (length(dots) == 0L) {
    return(empty_support())
  }
  supports <- lapply(dots, as_support_arg)
  a <- do.call(
    discretes::dsct_union,
    lapply(supports, function(s) s[["atoms"]])
  )
  intervals <- do.call(
    rbind,
    lapply(supports, function(s) s[["continuous"]])
  )
  if (is.null(intervals)) {
    intervals <- empty_intervals()
  }
  new_support(atoms = a, continuous = normalize_intervals(intervals))
}

#' Restrict a Support to an Interval
#'
#' Cut a support down to the part of it lying within `[from, to]`.
#'
#' @param support A support object, or a distribution.
#' @param ... Not used; must be empty. Present so that the arguments below are
#' matched by name.
#' @param from,to Endpoints of the interval to restrict to.
#' @param include_from,include_to Whether the endpoints themselves are kept.
#' @details
#' The `include_*` flags apply to the atoms only. An endpoint of a continuous
#' interval carries no probability either way, so including or excluding it
#' makes no difference to the continuous part.
#'
#' Restricting to a region the support does not reach gives
#' [empty_support()], which is the reason that object exists.
#' @returns A support object.
#' @seealso [support_union()] to combine supports instead.
#' @examples
#' support_restrict(continuous(c(0, 10)), from = 3, to = 6)
#' support_restrict(discrete(natural0()), to = 4)
#'
#' # Excluding an endpoint drops the atom sitting on it.
#' support_restrict(discrete(natural0()), to = 4, include_to = FALSE)
#'
#' # Restricting out of reach gives the empty support.
#' support_restrict(continuous(c(0, 1)), from = 5, to = 6)
#' @family Support algebra
#' @export
support_restrict <- function(support,
                             ...,
                             from = -Inf,
                             to = Inf,
                             include_from = TRUE,
                             include_to = TRUE) {
  rlang::check_dots_empty()
  s <- as_support_arg(support)
  checkmate::assert_number(from)
  checkmate::assert_number(to)
  a <- s[["atoms"]]
  intervals <- s[["continuous"]]
  if (discretes::num_discretes(a) > 0) {
    a <- discretes::dsct_keep(
      a,
      from = from,
      to = to,
      include_from = include_from,
      include_to = include_to
    )
  }
  if (nrow(intervals) > 0) {
    lo <- pmax(intervals[, "lower"], from)
    hi <- pmin(intervals[, "upper"], to)
    # A clipped interval `[a, a]` has measure zero, and an interval lying
    # entirely outside `[from, to]` gives `lo > hi`. Both are dropped.
    keep <- lo < hi
    intervals <- normalize_intervals(
      cbind(lower = unname(lo[keep]), upper = unname(hi[keep]))
    )
  }
  new_support(atoms = a, continuous = intervals)
}

#' Transform a Support
#'
#' Push a support through a strictly monotonic map, giving the support of the
#' transformed variable.
#'
#' @param support A support object, or a distribution.
#' @param fun,inv The map and its inverse. Both must be vectorised, and `fun`
#' must be strictly monotonic on the support.
#' @param ... Not used; must be empty. Present so that the arguments below are
#' matched by name.
#' @param increasing Whether `fun` is increasing. `FALSE` for a decreasing map,
#' which reverses each interval's endpoints.
#' @param domain,range The domain and range of `fun`, needed to transform an
#' atomic part that is described rather than enumerated.
#' @param by For `support_shift()` and `support_scale()`, the amount to shift
#' or scale by. Scaling by zero is not a monotonic map, and is an error.
#' @details
#' `support_shift()`, `support_scale()`, and `support_reciprocal()` are the
#' common cases, and avoid having to supply an inverse, a domain, and a range
#' by hand.
#'
#' `support_reciprocal()` maps each side of zero separately, since `1 / x` is
#' monotonic on each side but not across the two. A support with an atom at
#' zero has no reciprocal, and is an error. Zero lying inside a continuous
#' part is fine: a single point carries no probability there.
#' @returns A support object.
#' @examples
#' support_shift(continuous(c(0, 1)), by = 5)
#' support_scale(discrete(natural0()), by = 2)
#'
#' # A decreasing map reverses the interval.
#' support_scale(continuous(c(1, 2)), by = -1)
#'
#' # Reciprocal of a support spanning zero.
#' support_reciprocal(continuous(c(-2, 4)))
#'
#' # The general form.
#' support_transform(
#'   continuous(c(0, Inf)),
#'   fun = exp, inv = log,
#'   domain = c(0, Inf), range = c(1, Inf)
#' )
#' @family Support algebra
#' @export
support_transform <- function(support,
                              fun,
                              inv,
                              ...,
                              increasing = TRUE,
                              domain = c(-Inf, Inf),
                              range = c(-Inf, Inf)) {
  rlang::check_dots_empty()
  s <- as_support_arg(support)
  checkmate::assert_function(fun)
  checkmate::assert_function(inv)
  checkmate::assert_flag(increasing)
  a <- s[["atoms"]]
  intervals <- s[["continuous"]]
  if (discretes::num_discretes(a) > 0) {
    a <- discretes::dsct_transform(
      a,
      fun = fun,
      inv = inv,
      domain = domain,
      range = range,
      dir = if (increasing) "increasing" else "decreasing"
    )
  }
  if (nrow(intervals) > 0) {
    lo <- fun(intervals[, "lower"])
    hi <- fun(intervals[, "upper"])
    intervals <- if (increasing) {
      cbind(lower = unname(lo), upper = unname(hi))
    } else {
      cbind(lower = unname(hi), upper = unname(lo))
    }
    # A decreasing map reverses the order of the intervals, so re-canonicalize.
    intervals <- normalize_intervals(intervals)
  }
  new_support(atoms = a, continuous = intervals)
}

#' @rdname support_transform
#' @export
support_shift <- function(support, by) {
  checkmate::assert_number(by, finite = TRUE)
  support_transform(
    support,
    fun = function(x) x + by,
    inv = function(x) x - by,
    increasing = TRUE
  )
}

#' @rdname support_transform
#' @export
support_scale <- function(support, by) {
  checkmate::assert_number(by, finite = TRUE)
  if (by == 0) {
    stop(
      "Can't scale a support by zero: the result is a single point, which ",
      "is not a monotonic image of the original. Use `discrete(0)`."
    )
  }
  support_transform(
    support,
    fun = function(x) x * by,
    inv = function(x) x / by,
    increasing = by > 0
  )
}

#' @rdname support_transform
#' @export
support_reciprocal <- function(support) {
  s <- as_support_arg(support)
  if (isTRUE(support_has_atom(s, 0))) {
    stop(
      "Can't take the reciprocal of a support with an atom at zero, ",
      "because `1 / 0` is undefined."
    )
  }
  halves <- list(
    reciprocal_half(s, negative = TRUE),
    reciprocal_half(s, negative = FALSE)
  )
  support_union(halves)
}

#' One side of `support_reciprocal()`: restrict to the negative or positive
#' half-line, then map through `1 / x`, which is decreasing on each side.
#' @noRd
reciprocal_half <- function(support, negative) {
  s <- if (negative) {
    support_restrict(support, to = 0, include_to = FALSE)
  } else {
    support_restrict(support, from = 0, include_from = FALSE)
  }
  a <- s[["atoms"]]
  intervals <- s[["continuous"]]
  if (discretes::num_discretes(a) > 0) {
    side <- if (negative) c(-Inf, 0) else c(0, Inf)
    a <- discretes::dsct_transform(
      a,
      fun = function(x) 1 / x,
      inv = function(x) 1 / x,
      domain = side,
      range = side,
      dir = "decreasing"
    )
  }
  if (nrow(intervals) > 0) {
    # `1 / x` is decreasing on each side, so `[l, u]` maps to `[1/u, 1/l]`.
    # An endpoint at zero maps to the signed infinity of its own side.
    lo <- ifelse(intervals[, "upper"] == 0, -Inf, 1 / intervals[, "upper"])
    hi <- ifelse(intervals[, "lower"] == 0, Inf, 1 / intervals[, "lower"])
    intervals <- normalize_intervals(
      cbind(lower = unname(lo), upper = unname(hi))
    )
  }
  new_support(atoms = a, continuous = intervals)
}

#' Add or Remove Atoms
#'
#' Add atoms to a support, or take them away.
#'
#' @param support A support object, or a distribution.
#' @param atoms Atoms to add or remove: a numeric vector, or a `discretes`
#' object. Removing requires finitely many atoms, since they have to be
#' enumerated; adding does not.
#' @details
#' Adding an atom that is already there changes nothing. Removing one that is
#' not there changes nothing either. Removing an atom does not disturb the
#' continuous part, so removing an atom sitting on an interval leaves the
#' interval whole.
#' @returns A support object.
#' @examples
#' support_add_atoms(continuous(c(0, Inf)), 0)
#' support_drop_atoms(discrete(c(1, 2, 3)), 2)
#'
#' # Removing an atom leaves the continuous part alone.
#' support_drop_atoms(mixed(atoms = 0, continuous = c(0, 1)), 0)
#' @family Support algebra
#' @export
support_add_atoms <- function(support, atoms) {
  s <- as_support_arg(support)
  a <- as_atoms(atoms)
  new_support(
    atoms = discretes::dsct_union(s[["atoms"]], a),
    continuous = s[["continuous"]]
  )
}

#' @rdname support_add_atoms
#' @export
support_drop_atoms <- function(support, atoms) {
  s <- as_support_arg(support)
  values <- atoms_to_drop(atoms)
  a <- s[["atoms"]]
  for (v in values) {
    if (discretes::num_discretes(a) == 0) {
      break
    }
    a <- discretes::dsct_drop(a, from = v, to = v)
  }
  new_support(atoms = a, continuous = s[["continuous"]])
}

#' The values to remove, as a plain numeric vector. Removal has to enumerate,
#' so an infinite series can't be used here.
#' @noRd
atoms_to_drop <- function(atoms) {
  if (is.numeric(atoms)) {
    return(atoms)
  }
  if (inherits(atoms, "discretes")) {
    n <- discretes::num_discretes(atoms)
    if (!is.finite(n)) {
      stop(
        "Can't remove infinitely many atoms, because they have to be ",
        "enumerated. Use `support_restrict()` to cut a support down by ",
        "region instead."
      )
    }
    if (n == 0) {
      return(numeric(0))
    }
    bounds <- range(atoms)
    return(discretes::get_discretes_in(atoms, bounds[[1L]], bounds[[2L]]))
  }
  stop("Atoms must be a `discretes` object or a numeric vector.")
}

#' Test Membership of a Support
#'
#' Is a value in the support at all, or an atom of it specifically?
#'
#' @param support A support object, or a distribution.
#' @param at Values to test. Vectorised.
#' @details
#' `support_contains()` is `TRUE` for a value that is either an atom or inside
#' one of the continuous intervals. `support_has_atom()` is `TRUE` only for the
#' atoms, and so is the one to reach for when what matters is whether a point
#' carries positive probability.
#'
#' Continuous intervals count their endpoints as contained. Those endpoints
#' carry no probability, so a value can be contained in a support without being
#' a point of positive mass --- which is exactly the distinction between these
#' two functions.
#' @returns A logical vector the same length as `at`.
#' @examples
#' s <- mixed(atoms = 0, continuous = c(2, 5))
#' support_contains(s, at = c(0, 1, 3, 9))
#' support_has_atom(s, at = c(0, 1, 3, 9))
#'
#' support_has_atom(dst_pois(3), at = c(-1, 0, 2.5, 4))
#' @family Support algebra
#' @export
support_contains <- function(support, at) {
  s <- as_support_arg(support)
  checkmate::assert_numeric(at)
  res <- support_has_atom(s, at)
  intervals <- s[["continuous"]]
  for (i in seq_len(nrow(intervals))) {
    res <- res |
      (at >= intervals[i, "lower"] & at <= intervals[i, "upper"])
  }
  res
}

#' @rdname support_contains
#' @export
support_has_atom <- function(support, at) {
  s <- as_support_arg(support)
  checkmate::assert_numeric(at)
  if (discretes::num_discretes(s[["atoms"]]) == 0) {
    return(rep(FALSE, length(at)))
  }
  as.logical(discretes::has_discretes(s[["atoms"]], at))
}
