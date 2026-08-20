#' Specify the Support of a Distribution
#'
#' A distribution's *support* is the set on which it places probability.
#' It decomposes (the Lebesgue decomposition) into an *atomic* part --- a set
#' of points each carrying positive probability mass --- and a *continuous*
#' part --- a region carrying a density. `discrete()`, `continuous()`, and
#' `mixed()` construct a support from these pieces.
#'
#' @param atoms The atomic (discrete) part of the support: either a `discretes`
#' object (see the \pkg{discretes} package, e.g. [discretes::natural0()]) or a
#' numeric vector of finitely many atoms, which is coerced with
#' [discretes::as_discretes()]. A numeric vector is unambiguous here because the
#' argument names the intent (contrast with passing a bare vector to
#' `.support`, which is rejected).
#' @param continuous The continuous part of the support, as a union of closed
#' intervals. Provide one interval as a length-2 numeric `c(lower, upper)`, a
#' union as several such vectors, or a continuous support built by
#' `continuous()`.
#' @param ... For `continuous()`, one or more intervals, each a length-2 numeric
#' `c(lower, upper)`. With no arguments, `continuous()` defaults to the whole
#' real line, `c(-Inf, Inf)`. Overlapping or touching intervals are merged and
#' sorted into a canonical form.
#' @details
#' The variable type ([vtype()]) is *derived* from the support: a support with
#' only atoms is `"discrete"`, only a continuous part is `"continuous"`, and
#' both is `"mixed"`. `mixed()` therefore requires *both* parts to be non-empty;
#' use `discrete()` or `continuous()` for the pure cases.
#'
#' Intervals are treated as closed. Endpoints of a continuous part are
#' measure-zero, so open/closed makes no probabilistic difference there; an atom
#' that happens to sit on an interval boundary is simply tracked as an atom.
#' @returns A support object (class `"support"`).
#' @seealso [support()] to retrieve a distribution's support, [vtype()] for the
#' derived variable type.
#' @examples
#' discrete(discretes::natural0())   # e.g. the support of a Poisson
#' discrete(c(3.5, 1.2, 6.7))        # finitely many atoms
#' continuous(c(0, Inf))             # e.g. the support of a Gamma
#' continuous(c(0, 1), c(3, 4))      # a union of intervals
#' mixed(atoms = 0, continuous = c(0, Inf))  # an atom at 0 plus a tail
#' @family Support
#' @name support-construction
#' @export
discrete <- function(atoms) {
  a <- as_atoms(atoms)
  if (discretes::num_discretes(a) == 0) {
    stop("`discrete()` requires at least one atom.")
  }
  new_support(atoms = a)
}

#' @rdname support-construction
#' @export
continuous <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    dots <- list(c(-Inf, Inf))
  }
  new_support(continuous = normalize_intervals(collect_intervals(dots)))
}

#' @rdname support-construction
#' @export
mixed <- function(atoms, continuous) {
  a <- as_atoms(atoms)
  ci <- as_intervals(continuous)
  if (discretes::num_discretes(a) == 0) {
    stop(
      "`mixed()` requires a non-empty atomic part. ",
      "Use `continuous()` for a purely continuous support."
    )
  }
  if (nrow(ci) == 0) {
    stop(
      "`mixed()` requires a non-empty continuous part. ",
      "Use `discrete()` for a purely discrete support."
    )
  }
  new_support(atoms = a, continuous = ci)
}

#' The Empty Support
#'
#' A support containing nothing: no atoms and no continuous part.
#'
#' @details
#' No distribution has an empty support --- probability has to go somewhere ---
#' and [distribution()] rejects one. It exists so that operations on supports
#' are *closed*: restricting a support to a region it does not reach has to
#' return something, and that something is the empty support. It is also the
#' identity for taking unions.
#'
#' Its variable type is `"empty"`, which is a different claim from
#' `"unknown"`. Empty says there is nowhere to place probability; unknown says
#' nobody specified where.
#' @returns A support object (class `"support"`) with both parts empty.
#' @seealso [is_empty_support()] to test for it, [discrete()],
#' [continuous()], and [mixed()] for supports a distribution can actually have.
#' @examples
#' empty_support()
#' is_empty_support(empty_support())
#'
#' # The empty support is what falls out of an impossible restriction, and it
#' # is what `continuous()` returns when given no intervals at all.
#' continuous(numeric(0))
#' @family Support
#' @export
empty_support <- function() {
  new_support()
}

#' Low-Level Support Constructor
#'
#' Builds a support object from its already-validated parts. The atomic part is
#' a `discretes` object (possibly empty) and the continuous part is a canonical
#' interval matrix (possibly empty). This is the single representation that
#' `discrete()`, `continuous()`, and `mixed()` all produce; prefer those.
#'
#' @param atoms A `discretes` object.
#' @param continuous A two-column numeric matrix of intervals (`lower`,
#' `upper`), assumed already normalized.
#' @param ndim Number of dimensions. Always `1L` for now; reserved so that
#' multivariate supports (built by composing univariate ones) have somewhere to
#' record their dimension.
#' @returns A support object (class `"support"`).
#' @noRd
new_support <- function(atoms = discretes::empty_series(),
                        continuous = empty_intervals(),
                        ndim = 1L) {
  structure(
    list(atoms = atoms, continuous = continuous),
    ndim = ndim,
    class = "support"
  )
}

#' Coerce to a Support
#'
#' The single ingestion boundary that decides what counts as a support. Used by
#' `distribution()` (for `.support`) and, in future, by the multivariate
#' composition operators (per margin). A bare numeric vector is deliberately
#' *rejected*: `c(0, Inf)` is ambiguous between a continuous range and two atoms,
#' so the intent must be named via `discrete()` or `continuous()`.
#'
#' @param x A support object or a `discretes` object.
#' @returns A support object.
#' @noRd
as_support <- function(x) {
  if (is_support(x)) {
    return(x)
  }
  if (inherits(x, "discretes")) {
    return(new_support(atoms = x))
  }
  if (is.numeric(x)) {
    stop(
      "Can't interpret a bare numeric vector as a support, because ",
      "`c(0, Inf)` is ambiguous. Did you mean `discrete()` (atoms) or ",
      "`continuous()` (a range / intervals)?"
    )
  }
  stop("`.support` must be a support object or a `discretes` object.")
}

#' Test for a Support Object
#'
#' @param x Object to test.
#' @returns `TRUE` if `x` is a support object, otherwise `FALSE`.
#' @family Support
#' @export
is_support <- function(x) {
  inherits(x, "support")
}

#' @description
#' `is_empty_support()` tests whether a support is the empty one: no atoms and
#' no continuous part. It is `FALSE` for anything that is not a support.
#' @rdname is_support
#' @export
is_empty_support <- function(x) {
  if (!is_support(x)) {
    return(FALSE)
  }
  discretes::num_discretes(x[["atoms"]]) == 0 && nrow(x[["continuous"]]) == 0
}

#' Retrieve the Support of a Distribution
#'
#' Returns the structured support of a distribution: its atomic part and its
#' continuous part. Every distribution has one, because [distribution()]
#' requires it --- the single exception being [dst_null()], which has nothing to
#' place anywhere and returns `NULL`.
#'
#' @param distribution Distribution object.
#' @returns A support object, or `NULL` if the distribution has no structured
#' support.
#' @seealso [discrete()], [continuous()], [mixed()] to build supports;
#' [vtype()] for the derived variable type.
#' @examples
#' support(distribution(.support = continuous(c(0, Inf))))
#' @family Support
#' @export
support <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  attributes(distribution)[["support"]]
}

#' Atomic and Continuous Parts of a Support
#'
#' Extract the atomic (discrete) part or the continuous part of a support. Each
#' accepts either a support object or a distribution.
#'
#' @param x A support object or a distribution.
#' @returns For `atoms()`, a `discretes` object. For `continuous_part()`, a
#' two-column numeric matrix of intervals (`lower`, `upper`).
#' @examples
#' atoms(mixed(atoms = 0, continuous = c(0, Inf)))
#' continuous_part(continuous(c(0, 1), c(3, 4)))
#' @family Support
#' @export
atoms <- function(x) {
  as_support_arg(x)[["atoms"]]
}

#' @rdname atoms
#' @export
continuous_part <- function(x) {
  as_support_arg(x)[["continuous"]]
}

#' @export
print.support <- function(x, ...) {
  cat(sprintf("<support: %s>\n", vtype_of_support(x)))
  if (discretes::num_discretes(x[["atoms"]]) > 0) {
    cat("-- atoms --\n")
    print(x[["atoms"]])
  }
  if (nrow(x[["continuous"]]) > 0) {
    ints <- apply(
      x[["continuous"]], 1L,
      function(r) sprintf("[%g, %g]", r[[1L]], r[[2L]])
    )
    cat("-- continuous --\n", paste(ints, collapse = " U "), "\n", sep = "")
  }
  invisible(x)
}

# ---- internal helpers -------------------------------------------------------

#' The variable type implied by a support.
#' @noRd
vtype_of_support <- function(support) {
  has_atoms <- discretes::num_discretes(support[["atoms"]]) > 0
  has_cont <- nrow(support[["continuous"]]) > 0
  if (has_atoms && has_cont) {
    return("mixed")
  }
  if (has_atoms) {
    return("discrete")
  }
  if (has_cont) {
    return("continuous")
  }
  # Neither part is present: the support is empty. This is a different claim
  # from "unknown", which is what a distribution reports when nobody said.
  "empty"
}

#' The hull (min, max) of a support, used to derive a distribution's range.
#' @noRd
support_hull <- function(support) {
  los <- numeric(0)
  his <- numeric(0)
  if (discretes::num_discretes(support[["atoms"]]) > 0) {
    r <- range(support[["atoms"]])
    los <- c(los, r[[1L]])
    his <- c(his, r[[2L]])
  }
  if (nrow(support[["continuous"]]) > 0) {
    los <- c(los, min(support[["continuous"]][, "lower"]))
    his <- c(his, max(support[["continuous"]][, "upper"]))
  }
  if (length(los) == 0) {
    return(c(NA_real_, NA_real_))
  }
  c(min(los), max(his))
}

#' Coerce atoms input (a discretes object or numeric) to a discretes object.
#' @noRd
as_atoms <- function(x) {
  if (inherits(x, "discretes")) {
    return(x)
  }
  if (is.numeric(x)) {
    return(discretes::as_discretes(x))
  }
  stop("Atoms must be a `discretes` object or a numeric vector.")
}

#' Coerce a `continuous` argument (intervals or a continuous support) to a
#' canonical interval matrix.
#' @noRd
as_intervals <- function(x) {
  if (is_support(x)) {
    if (discretes::num_discretes(x[["atoms"]]) > 0) {
      stop(
        "The `continuous` part must be a purely continuous support, ",
        "not a discrete or mixed one."
      )
    }
    return(x[["continuous"]])
  }
  dots <- if (is.list(x) && !is.matrix(x)) x else list(x)
  normalize_intervals(collect_intervals(dots))
}

#' Accept a support or a distribution, returning the support.
#' @noRd
as_support_arg <- function(x) {
  if (is_support(x)) {
    return(x)
  }
  if (inherits(x, "dst")) {
    s <- support(x)
    if (is.null(s)) {
      stop(
        "This distribution has no structured support. Only the Null ",
        "distribution has none; every other distribution declares one."
      )
    }
    return(s)
  }
  stop("Expected a support object or a distribution.")
}

#' An empty continuous part: a 0-row interval matrix.
#' @noRd
empty_intervals <- function() {
  m <- matrix(numeric(0), ncol = 2L)
  colnames(m) <- c("lower", "upper")
  m
}

#' Gather `...`-style interval inputs into a two-column matrix.
#' @noRd
collect_intervals <- function(dots) {
  # A single list-of-intervals argument: unwrap it.
  if (length(dots) == 1L && is.list(dots[[1L]]) && !is.matrix(dots[[1L]])) {
    dots <- dots[[1L]]
  }
  # A single matrix argument: its rows are the intervals.
  if (length(dots) == 1L && is.matrix(dots[[1L]])) {
    x <- dots[[1L]]
    if (ncol(x) != 2L) {
      stop("An interval matrix must have two columns (lower, upper).")
    }
    return(x)
  }
  # Drop empty entries (e.g. `numeric(0)`) so "no continuous part" is
  # representable; callers decide whether that is allowed.
  dots <- Filter(function(v) !(is.numeric(v) && length(v) == 0L), dots)
  if (length(dots) == 0L) {
    return(empty_intervals())
  }
  rows <- lapply(dots, function(v) {
    if (!is.numeric(v) || length(v) != 2L) {
      stop("Each interval must be a length-2 numeric vector `c(lower, upper)`.")
    }
    as.numeric(v)
  })
  do.call(rbind, rows)
}

#' Validate, sort, and merge an interval matrix into a canonical disjoint form.
#' @noRd
normalize_intervals <- function(m) {
  if (nrow(m) == 0) {
    return(empty_intervals())
  }
  if (anyNA(m)) {
    stop("Interval endpoints must not be `NA`.")
  }
  if (any(m[, 1L] >= m[, 2L])) {
    stop("Each interval must have `lower < upper`.")
  }
  # Drop degenerate intervals `[a, a]`: a continuous part on a single point has
  # measure zero (no probability mass), so it is not part of the canonical form.
  m <- m[m[, 1L] < m[, 2L], , drop = FALSE]
  if (nrow(m) == 0) {
    return(empty_intervals())
  }
  ord <- order(m[, 1L], m[, 2L])
  m <- m[ord, , drop = FALSE]
  lo <- m[1L, 1L]
  hi <- m[1L, 2L]
  out <- list()
  for (i in seq_len(nrow(m))[-1L]) {
    if (m[i, 1L] <= hi) {
      # Overlapping or touching (closed intervals): extend the current run.
      hi <- max(hi, m[i, 2L])
    } else {
      out[[length(out) + 1L]] <- c(lo, hi)
      lo <- m[i, 1L]
      hi <- m[i, 2L]
    }
  }
  out[[length(out) + 1L]] <- c(lo, hi)
  res <- do.call(rbind, out)
  # Normalize negative zero (e.g. from negating an endpoint at 0) so it does not
  # surface as "-0" in printing or comparisons.
  res[res == 0] <- 0
  colnames(res) <- c("lower", "upper")
  res
}
