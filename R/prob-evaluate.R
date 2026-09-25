# Finding the probability of an event built in `prob-events.R`.
#
# Three ways, tried in order: evaluate the event at every point of a finite
# distribution; walk the atoms of a univariate discrete one; or turn the
# event into a union of boxes in the quantities it compares, and add up
# box probabilities from the joint distribution of those quantities.

# ---- evaluating --------------------------------------------------------------

#' Probability of an event with recycled values.
#' @noRd
prob_event <- function(distribution, ev) {
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts) && has_stated(distribution, "pmf")) {
    return(prob_event_points(distribution, pts, ev))
  }
  if (!is_multivariate(distribution)) {
    fin <- finite_points_of(distribution)
    if (!is.null(fin)) {
      return(prob_event_points(distribution, fin, ev))
    }
    if (vtype(distribution) == "discrete" && has_nonlinear(ev)) {
      return(prob_event_atoms(distribution, ev))
    }
  }
  prob_event_boxes(distribution, ev)
}

#' Does an event compare any quantity that is not a sum of multiples of the
#' variables?
#' @noRd
has_nonlinear <- function(ev) {
  if (ev$type == "atom") {
    return(!term_linear(ev$term))
  }
  if (ev$type == "const") {
    return(FALSE)
  }
  any(vapply(ev$children, has_nonlinear, logical(1)))
}

#' Probability of any event for a univariate discrete distribution, by
#' adding up the atoms where it holds, walking outward through infinitely
#' many until the rest are negligible (as for moments).
#' @noRd
prob_event_atoms <- function(distribution, ev) {
  name <- variables(distribution)
  n <- length(event_values(ev)[[1L]])
  vapply(seq_len(n), function(i) {
    if (anyNA(vapply(event_values(ev), `[[`, numeric(1), i))) {
      return(NA_real_)
    }
    indicator <- function(xs) {
      env <- stats::setNames(list(xs), name)
      as.numeric(event_holds(attach_point_values(ev, env), i))
    }
    expect_over_support(distribution, indicator)
  }, numeric(1))
}

#' The points of a univariate distribution with finitely many atoms and
#' nothing else, as a one-column data frame; otherwise `NULL`.
#' @noRd
finite_points_of <- function(distribution) {
  s <- support(distribution)
  if (is.null(s) || nrow(s[["continuous"]]) > 0) {
    return(NULL)
  }
  if (!is.finite(discretes::num_discretes(s[["atoms"]]))) {
    return(NULL)
  }
  out <- data.frame(discretes::get_discretes_in(s[["atoms"]]))
  names(out) <- variables(distribution)
  out
}

#' Evaluate an event at each point of a finite distribution.
#' @noRd
prob_event_points <- function(distribution, pts, ev) {
  probs <- eval_joint(distribution, "pmf", as.list(pts))
  env <- as.list(pts)
  names(env) <- variables(distribution)
  ev <- attach_point_values(ev, env)
  n <- length(event_values(ev)[[1L]])
  vapply(seq_len(n), function(i) {
    holds <- event_holds(ev, i)
    if (anyNA(holds)) {
      return(NA_real_)
    }
    sum(probs[holds])
  }, numeric(1))
}

#' Work out each atom's quantity at every point, once.
#' @noRd
attach_point_values <- function(ev, env) {
  if (ev$type == "atom") {
    ev$at_points <- ev$term$fun(env)
    return(ev)
  }
  if (ev$type == "const") {
    return(ev)
  }
  ev$children <- lapply(ev$children, attach_point_values, env = env)
  ev
}

#' Which points satisfy an event, for the `i`th value.
#' @noRd
event_holds <- function(ev, i) {
  switch(ev$type,
    atom = get(ev$op)(ev$at_points, ev$value[[i]]),
    const = ev$value[[i]],
    and = Reduce(`&`, lapply(ev$children, event_holds, i = i)),
    or = Reduce(`|`, lapply(ev$children, event_holds, i = i)),
    not = !event_holds(ev$children[[1L]], i)
  )
}

#' Probability of an event, as a union of boxes in the quantities it
#' compares.
#' @noRd
prob_event_boxes <- function(distribution, ev) {
  n <- length(event_values(ev)[[1L]])
  coords <- event_coordinates(list(list(atoms = event_atoms(ev))))
  if (length(coords) > 0L) {
    dist_y <- coordinate_distribution(distribution, coords)
    ev <- drop_null_comparisons(ev, dist_y, coords)
  }
  clauses <- event_dnf(ev, n)
  if (length(clauses) == 0L) {
    return(rep(0, n))
  }
  if (length(clauses) > 12L) {
    stop(
      "This event breaks into ", length(clauses), " pieces, too many to\n",
      "add up. Try writing it more simply.",
      call. = FALSE
    )
  }
  if (length(event_coordinates(clauses)) == 0L) {
    # No variables left in it: the event holds or it does not.
    held <- Reduce(`|`, lapply(clauses, `[[`, "mask"))
    return(as.numeric(held))
  }
  total <- rep(0, n)
  m <- length(clauses)
  # Inclusion-exclusion over the pieces: each intersection of boxes is a
  # box.
  for (k in seq_len(2^m - 1L)) {
    pick <- which(bitwAnd(k, as.integer(2^(seq_len(m) - 1L))) > 0)
    atoms <- unlist(lapply(clauses[pick], `[[`, "atoms"), recursive = FALSE)
    mask <- Reduce(`&`, lapply(clauses[pick], `[[`, "mask"))
    p <- box_prob(dist_y, coords, atoms, mask, n)
    total <- total + (-1)^(length(pick) + 1L) * p
  }
  pmin(pmax(total, 0), 1)
}

#' The comparisons in an event.
#' @noRd
event_atoms <- function(ev) {
  if (ev$type == "atom") {
    return(list(ev))
  }
  if (ev$type == "const") {
    return(list())
  }
  unlist(lapply(ev$children, event_atoms), recursive = FALSE)
}

#' Replace `==` and `!=` on quantities without atoms by what they almost
#' surely are.
#'
#' A quantity with no atoms equals any given value with probability zero,
#' so `==` is false and `!=` true, as far as probability goes. Settling them
#' before expanding the event keeps each `!=` from doubling the number of
#' pieces to add up. Whether a quantity has atoms is read from its own
#' distribution: `x - y` can have one even when `x` and `y` do not.
#' @noRd
drop_null_comparisons <- function(ev, dist_y, coords) {
  keys <- vapply(coords, `[[`, "", "key")
  s <- support(dist_y)
  atomless <- vapply(seq_along(coords), function(j) {
    sj <- if (length(coords) == 1L) s else support_marginal(s, j)
    vtype_of_support(sj) == "continuous"
  }, logical(1))
  settle <- function(e) {
    if (e$type == "atom") {
      if (e$op %in% c("==", "!=") && term_linear(e$term)) {
        j <- match(coord_key(e$term$coef), keys)
        if (atomless[[j]]) {
          return(event_const(rep(e$op == "!=", length(e$value)), e$ids))
        }
      }
      return(e)
    }
    if (e$type == "const") {
      return(e)
    }
    e$children <- lapply(e$children, settle)
    e
  }
  settle(ev)
}

#' Push negations to the comparisons, and expand into an "or" of "and"s.
#'
#' Each piece ("clause") is a list of comparisons that must all hold, and
#' a mask of the values for which it can hold at all.
#' @noRd
event_dnf <- function(ev, n) {
  ev <- push_not(ev, negate = FALSE)
  to_dnf(ev, n)
}

#' @noRd
push_not <- function(ev, negate) {
  if (ev$type == "not") {
    return(push_not(ev$children[[1L]], !negate))
  }
  if (ev$type == "const") {
    if (negate) {
      ev$value <- !ev$value
    }
    return(ev)
  }
  if (ev$type %in% c("and", "or")) {
    type <- ev$type
    if (negate) {
      type <- if (type == "and") "or" else "and"
    }
    kids <- lapply(ev$children, push_not, negate = negate)
    return(event_node(type, kids))
  }
  op <- ev$op
  if (negate) {
    op <- switch(op,
      "<" = ">=",
      "<=" = ">",
      ">" = "<=",
      ">=" = "<",
      "==" = "!=",
      "!=" = "=="
    )
  }
  if (op == "!=") {
    lower <- ev
    upper <- ev
    lower$op <- "<"
    upper$op <- ">"
    return(event_node("or", list(lower, upper)))
  }
  ev$op <- op
  ev
}

#' @noRd
to_dnf <- function(ev, n) {
  if (ev$type == "atom") {
    return(list(list(atoms = list(ev), mask = rep(TRUE, n))))
  }
  if (ev$type == "const") {
    return(list(list(atoms = list(), mask = ev$value)))
  }
  kids <- lapply(ev$children, to_dnf, n = n)
  if (ev$type == "or") {
    return(unlist(kids, recursive = FALSE))
  }
  Reduce(function(a, b) {
    out <- list()
    for (ca in a) {
      for (cb in b) {
        out[[length(out) + 1L]] <- list(
          atoms = c(ca$atoms, cb$atoms),
          mask = ca$mask & cb$mask
        )
      }
    }
    out
  }, kids)
}

#' The distinct quantities compared in some clauses.
#' @returns A list with `key` (identifying each), `coef` (their
#' coefficients), and `label`.
#' @noRd
event_coordinates <- function(clauses) {
  atoms <- unlist(lapply(clauses, `[[`, "atoms"), recursive = FALSE)
  keys <- character(0)
  out <- list()
  for (a in atoms) {
    if (!term_linear(a$term)) {
      stop(
        "Cannot find the probability of an event on `", a$term$label,
        "`.\nOnly sums of multiples of the variables can be handled for\n",
        "this distribution; any condition can be, for one on finitely\n",
        "many points.",
        call. = FALSE
      )
    }
    key <- coord_key(a$term$coef)
    if (!key %in% keys) {
      keys <- c(keys, key)
      out[[length(out) + 1L]] <- list(
        key = key,
        coef = a$term$coef,
        label = a$term$label
      )
    }
  }
  out
}

#' @noRd
coord_key <- function(coef) {
  paste(names(coef), format(coef, digits = 15), sep = ":", collapse = "|")
}

#' The joint distribution of the quantities compared in an event.
#' @noRd
coordinate_distribution <- function(distribution, coords) {
  vars <- variables(distribution)
  unit <- vapply(coords, function(cd) {
    length(cd$coef) == 1L && cd$coef[[1L]] == 1
  }, logical(1))
  if (all(unit)) {
    idx <- match(vapply(coords, function(cd) names(cd$coef), ""), vars)
    return(marginal(distribution, idx))
  }
  a <- matrix(0, nrow = length(coords), ncol = length(vars))
  colnames(a) <- vars
  for (j in seq_along(coords)) {
    a[j, names(coords[[j]]$coef)] <- coords[[j]]$coef
  }
  rownames(a) <- paste0(".q", seq_along(coords))
  out <- eval_property(distribution, "linear", a)
  if (is.null(out)) {
    labels <- vapply(coords[!unit], `[[`, "", "label")
    stop(
      "Cannot find the probability of an event on `", labels[[1L]],
      "`,\nwhich combines several variables. This distribution does not\n",
      "state a `linear` property giving the distribution of such a\n",
      "combination.",
      call. = FALSE
    )
  }
  out
}

#' Probability that the quantities lie in the box set by some comparisons.
#' @param dist_y Joint distribution of the quantities, one per coordinate.
#' @param atoms Comparisons, each on one of the coordinates.
#' @param mask Logical, for which values the box can be non-empty at all.
#' @noRd
box_prob <- function(dist_y, coords, atoms, mask, n) {
  k <- length(coords)
  keys <- vapply(coords, `[[`, "", "key")
  lo <- rep(list(rep(-Inf, n)), k)
  hi <- rep(list(rep(Inf, n)), k)
  lo_open <- rep(list(rep(TRUE, n)), k)
  hi_open <- rep(list(rep(FALSE, n)), k)
  for (a in atoms) {
    j <- match(coord_key(a$term$coef), keys)
    v <- a$value
    if (a$op %in% c("<", "<=", "==")) {
      open <- a$op == "<"
      tighter <- v < hi[[j]]
      same <- v == hi[[j]]
      hi_open[[j]] <- ifelse(tighter, open, hi_open[[j]] | (same & open))
      hi[[j]] <- pmin(hi[[j]], v)
    }
    if (a$op %in% c(">", ">=", "==")) {
      open <- a$op == ">"
      tighter <- v > lo[[j]]
      same <- v == lo[[j]]
      lo_open[[j]] <- ifelse(tighter, open, lo_open[[j]] | (same & open))
      lo[[j]] <- pmax(lo[[j]], v)
    }
  }
  empty <- !mask
  for (j in seq_len(k)) {
    empty <- empty | lo[[j]] > hi[[j]] |
      (lo[[j]] == hi[[j]] & (lo_open[[j]] | hi_open[[j]]) &
        is.finite(lo[[j]]))
  }
  out <- if (k == 1L) {
    interval_prob(dist_y, lo[[1L]], lo_open[[1L]], hi[[1L]], hi_open[[1L]])
  } else {
    rectangle_prob(dist_y, lo, lo_open, hi, hi_open)
  }
  out[empty %in% TRUE] <- 0
  out
}

#' Probability of an interval for one quantity, exactly, whatever its type:
#' the atoms at the ends are added or removed with the PMF.
#' @noRd
interval_prob <- function(d, lo, lo_open, hi, hi_open) {
  # Evaluate only where needed: a distribution with atoms may not be able to
  # give a PMF everywhere, and there is no need to ask where no end is shut.
  where <- function(fun, x, need) {
    out <- rep(0, length(x))
    need <- which(need)
    if (length(need) > 0L) {
      out[need] <- fun(d, x[need])
    }
    out
  }
  pmf_hi <- where(eval_pmf, hi, hi_open & is.finite(hi))
  pmf_lo <- where(eval_pmf, lo, !lo_open & is.finite(lo))
  if (all(hi == Inf)) {
    # Only a lower bound: use the survival function, which keeps its
    # precision far out in the upper tail.
    above_lo <- where(eval_survival, lo, is.finite(lo))
    above_lo[lo == -Inf] <- 1
    return(pmin(pmax(above_lo + pmf_lo, 0), 1))
  }
  below_hi <- eval_cdf(d, hi)
  below_lo <- where(eval_cdf, lo, is.finite(lo))
  pmin(pmax((below_hi - pmf_hi) - (below_lo - pmf_lo), 0), 1)
}

#' Probability of a box for several quantities, by inclusion-exclusion on
#' the CDF (or on the survival function, for a box bounded only below).
#'
#' Ends that exclude atoms are first moved to the neighbouring atom, which
#' needs each quantity's support to be purely continuous or discrete.
#' @noRd
rectangle_prob <- function(d, lo, lo_open, hi, hi_open) {
  k <- length(lo)
  s <- support(d)
  for (j in seq_len(k)) {
    sj <- support_marginal(s, j)
    # `y < b` is `y <= b-`; `y >= a` is `y > a-`.
    shut_hi <- which(hi_open[[j]] & is.finite(hi[[j]]))
    hi[[j]][shut_hi] <- step_below(sj, hi[[j]][shut_hi])
    shut_lo <- which(!lo_open[[j]] & is.finite(lo[[j]]))
    lo[[j]][shut_lo] <- step_below(sj, lo[[j]][shut_lo])
  }
  if (all(vapply(hi, function(v) all(v == Inf), logical(1)))) {
    return(eval_joint(d, "survival", lo))
  }
  bounded <- which(vapply(lo, function(v) any(v > -Inf), logical(1)))
  total <- 0
  for (m in seq_len(2^length(bounded)) - 1L) {
    use_lo <- bounded[bitwAnd(m, as.integer(2^(seq_along(bounded) - 1L))) > 0]
    z <- hi
    z[use_lo] <- lo[use_lo]
    total <- total + (-1)^length(use_lo) * eval_joint(d, "cdf", z)
  }
  pmin(pmax(total, 0), 1)
}

#' For each value, the point just below it in a univariate support.
#'
#' For a continuous variable, the value itself, because there is no atom
#' there to step past. For a discrete one, the largest atom below the value
#' (or `-Inf` if there is none).
#' @noRd
step_below <- function(s, x) {
  type <- vtype_of_support(s)
  if (type == "continuous") {
    return(x)
  }
  if (type != "discrete") {
    stop(
      "A strict inequality on a variable that has both atoms and a\n",
      "continuous part is not supported yet.\n",
      "Use `<=` or `>` for that variable."
    )
  }
  vapply(x, function(v) {
    if (is.na(v)) {
      return(NA_real_)
    }
    below <- discretes::prev_discrete(s[["atoms"]], from = v)
    if (length(below) == 0) -Inf else below
  }, numeric(1))
}
