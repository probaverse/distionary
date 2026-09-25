#' Probability of an Event
#'
#' The probability that a distribution's variables satisfy a condition,
#' written the way you would write it for `dplyr::filter()`: `x < 2`,
#' `x <= 1 & y > 3`, `x > 100 | y > 100`. Optionally, conditional on
#' another.
#'
#' @param distribution A distribution.
#' @param event <[`data-masking`][rlang::args_data_masking]> A condition on
#' the distribution's variables, referred to by name (see [variables()]).
#' Combine comparisons with `&`, `|`, and `!`. A variable left out of the
#' event is unrestricted.
#' @param ... Not used; forces `given` to be named.
#' @param given <[`data-masking`][rlang::args_data_masking]> Optionally, a
#' condition to be given (to hold). A variable compared with `==` is
#' conditioned on taking that value; anything else is conditioned on as an
#' event.
#' @details
#' ## Writing events
#'
#' Refer to the variables by name. A univariate distribution's variable is
#' called `x` unless it has been named otherwise. Values come from where
#' `prob()` is called, as in dplyr; where a value has the same name as a
#' variable, write `.env$name` for the value.
#'
#' Values may be vectors, and the result then has one probability per
#' element, recycled as in [vctrs::vec_recycle_common()]:
#' `prob(d, x < c(1, 2, 3))` gives three probabilities.
#'
#' A variable left out is free, so there is no need to take a [marginal()]
#' first: for a distribution of `x` and `y`, `prob(d, x < 2)` is
#' \eqn{P(X < 2)}.
#'
#' ## Conditioning
#'
#' `given = x == 1` conditions on \eqn{X} taking the value 1, through the
#' conditional distribution; this is how to condition on a value of a
#' continuous variable, which has probability zero. Any other condition is
#' an event, and is divided out: `prob(d, y > 3, given = x > 1)` is
#' \eqn{P(X > 1, Y > 3) / P(X > 1)}. Both may be combined with `&`, as in
#' `given = x == 1 & z > 0`. The event cannot mention a variable that
#' `given` fixes to a value.
#'
#' A combination of variables can be given a value too, which slices the
#' distribution: `prob(d, x > 60, given = x + y == 200)` is the probability
#' that \eqn{X} exceeds 60 when \eqn{X + Y = 200}. This needs the
#' distribution's `linear` property, as for events on combinations.
#'
#' ## What can be evaluated
#'
#' Every comparison in an event is a condition on one quantity: `x < y` is
#' a condition on `x - y`, and `x + y > s` one on `x + y`. An event is
#' then a region bounded in each of those quantities, so its probability
#' comes from their joint distribution. Which quantities are available
#' depends on the distribution:
#'
#' - For one on finitely many points, anything: the event is evaluated at
#'   each point, as `filter()` evaluates it on each row, and the
#'   probabilities of the points satisfying it are added up. The same goes
#'   for a discrete distribution of one variable with infinitely many
#'   atoms, walking out through them until the rest are negligible.
#' - Where the distribution states a `linear` property, as the multivariate
#'   Normal and t do, any sum of multiples of the variables, such as
#'   `x - y` or `x + y`.
#' - Otherwise, the variables themselves.
#'
#' An event needing something else is refused, naming the quantity that
#' could not be found, rather than approximated. All answers are exact, to
#' within the accuracy of the distribution's CDF.
#'
#' Strict and non-strict inequalities differ only where there are atoms,
#' and are handled exactly for a single quantity, and for several whose
#' supports are each continuous or discrete.
#' @returns A numeric vector of probabilities, with the common length of the
#' values in `event` and `given`.
#' @seealso [prob_left()] and [prob_right()] for a single variable;
#' [eval_mv_cdf()] for the CDF as a function.
#' @examples
#' d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
#' prob(d, x < 0)
#' prob(d, x < 0 & y > 1)
#' prob(d, x > 2 | y > 5)
#' prob(d, 0 < x & x <= 1)
#' prob(d, y > c(1, 3, 5))
#'
#' # Combinations of variables.
#' prob(d, x + y > 3)
#' prob(d, x < y)
#'
#' # Conditioning on a value, or on an event.
#' prob(d, y > 3, given = x == 1)
#' prob(d, y > 3, given = x > 1)
#'
#' # A slice: x and y, when their total is 5.
#' prob(d, x > 2, given = x + y == 5)
#'
#' # A univariate distribution's variable is `x`.
#' prob(dst_pois(3), x >= 2 & x != 4)
#' @export
prob <- function(distribution, event, ..., given = NULL) {
  rlang::check_dots_empty()
  checkmate::assert_class(distribution, "dst")
  vars <- variables(distribution)
  if (is.null(vars)) {
    vars <- "x"
  }
  mask <- event_mask(vars)
  ev <- as_event(
    rlang::eval_tidy(rlang::enquo(event), data = mask),
    "event"
  )
  gv <- rlang::eval_tidy(rlang::enquo(given), data = mask)
  gv <- if (is.null(gv)) NULL else as_event(gv, "given")
  n <- event_length(list(ev, gv))
  if (is.na(distribution)) {
    return(rep(NA_real_, n))
  }
  ev <- event_recycle(ev, n)
  if (is.null(gv)) {
    return(prob_event(distribution, ev))
  }
  gv <- event_recycle(gv, n)
  prob_given(distribution, ev, gv, n)
}

# ---- terms: what a variable, or arithmetic on variables, evaluates to -----

#' Bind each variable name to a term, for evaluating an event.
#' @noRd
event_mask <- function(vars) {
  stats::setNames(lapply(vars, variable_term), vars)
}

#' A term: a quantity computed from the variables.
#'
#' A linear term is `sum(coef * variables) + const`, with `coef` a named
#' vector of single numbers. A non-linear term has `coef = NULL`, and is
#' known only through `fun`. Either way, `fun(env)` evaluates the part
#' other than `const` on a named list of variable values, and `const` may
#' be a vector (one value per probability asked for).
#' @noRd
new_term <- function(coef, const, fun, label) {
  structure(
    list(coef = coef, const = const, fun = fun, label = label),
    class = c("dst_term", "dst_expr")
  )
}

#' @noRd
variable_term <- function(name) {
  new_term(
    coef = stats::setNames(1, name),
    const = 0,
    fun = function(env) env[[name]],
    label = name
  )
}

#' A number, as a term with no variables.
#' @noRd
as_term <- function(x) {
  if (inherits(x, "dst_term")) {
    return(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop(
      "An event compares variables with numbers, not with ",
      class(x)[[1L]], " values.",
      call. = FALSE
    )
  }
  new_term(
    coef = stats::setNames(numeric(0), character(0)),
    const = as.numeric(x),
    fun = function(env) 0,
    label = format_value(x)
  )
}

#' @noRd
has_variables <- function(t) {
  is.null(t$coef) || length(t$coef) > 0
}

#' @noRd
term_linear <- function(t) {
  !is.null(t$coef)
}

#' Value of a term at the points in `env`, for building a non-linear term.
#' @noRd
term_value <- function(t, env) {
  t$fun(env) + t$const
}

#' @noRd
format_value <- function(x) {
  if (length(x) == 1L) format(x) else "<vector>"
}

#' Sum of two terms.
#' @noRd
term_add <- function(t1, t2, label) {
  const <- t1$const + t2$const
  f1 <- t1$fun
  f2 <- t2$fun
  fun <- function(env) f1(env) + f2(env)
  if (term_linear(t1) && term_linear(t2)) {
    nms <- union(names(t1$coef), names(t2$coef))
    coef <- stats::setNames(numeric(length(nms)), nms)
    coef[names(t1$coef)] <- coef[names(t1$coef)] + t1$coef
    coef[names(t2$coef)] <- coef[names(t2$coef)] + t2$coef
    coef <- coef[coef != 0]
    return(new_term(coef, const, fun, label))
  }
  new_term(NULL, const, fun, label)
}

#' A term times a single number.
#' @noRd
term_scale <- function(t, k, label) {
  f <- t$fun
  coef <- if (term_linear(t)) t$coef * k else NULL
  if (!is.null(coef)) {
    coef <- coef[coef != 0]
  }
  new_term(coef, t$const * k, function(env) f(env) * k, label)
}

#' A term built by a function of the variables that is not linear.
#'
#' Its constant parts are folded into the function, so they must be single
#' numbers: a vector of constants inside, say, `exp()` would make a
#' different quantity for each probability asked for.
#' @noRd
term_nonlinear <- function(op, terms, label, args = list()) {
  for (t in terms) {
    if (length(t$const) != 1L) {
      stop(
        "In `", label, "`, a vector of values is used inside a\n",
        "calculation on the variables. Values can vary only where they\n",
        "are added to, or compared with, the variables.",
        call. = FALSE
      )
    }
  }
  fun <- function(env) {
    vals <- lapply(terms, term_value, env = env)
    do.call(op, c(vals, args))
  }
  new_term(NULL, 0, fun, label)
}

#' Arithmetic on terms.
#' @noRd
term_arith <- function(op, t1, t2, label) {
  v1 <- has_variables(t1)
  v2 <- has_variables(t2)
  if (!v1 && !v2) {
    out <- as_term(get(op)(t1$const, t2$const))
    out$label <- label
    return(out)
  }
  if (op == "+") {
    return(term_add(t1, t2, label))
  }
  if (op == "-") {
    return(term_add(t1, term_scale(t2, -1, t2$label), label))
  }
  if (op == "*" && (!v1 || !v2)) {
    k <- if (v1) t2$const else t1$const
    t <- if (v1) t1 else t2
    if (length(k) != 1L) {
      stop(
        "In `", label, "`, a variable is multiplied by a vector.\n",
        "Multiply variables by single numbers.",
        call. = FALSE
      )
    }
    return(term_scale(t, k, label))
  }
  if (op == "/" && !v2) {
    if (length(t2$const) != 1L) {
      stop(
        "In `", label, "`, a variable is divided by a vector.\n",
        "Divide variables by single numbers.",
        call. = FALSE
      )
    }
    return(term_scale(t1, 1 / t2$const, label))
  }
  term_nonlinear(op, list(t1, t2), label)
}

# ---- events ---------------------------------------------------------------

#' @noRd
new_event <- function(node) {
  structure(node, class = c("dst_event", "dst_expr"))
}

#' @noRd
event_atom <- function(term, op, value) {
  new_event(list(type = "atom", term = term, op = op, value = value))
}

#' @noRd
event_const <- function(value) {
  new_event(list(type = "const", value = as.logical(value)))
}

#' @noRd
event_node <- function(type, children) {
  new_event(list(type = type, children = children))
}

#' A comparison of two terms, as a condition on their difference.
#'
#' The difference `lhs - rhs` is compared with zero, moving its constant to
#' the other side. A linear difference is put in a canonical form (its
#' variables in order of name, the first with coefficient 1), so that the
#' same quantity is recognised however it is written: `x - y < 0` and
#' `y > x` are both conditions on `x - y`.
#' @noRd
term_compare <- function(op, t1, t2, label) {
  d <- term_arith("-", t1, t2, label)
  if (!has_variables(d)) {
    return(event_const(get(op)(d$const, 0)))
  }
  value <- -d$const
  if (term_linear(d)) {
    coef <- d$coef[order(names(d$coef))]
    lead <- coef[[1L]]
    coef <- coef / lead
    value <- value / lead
    if (lead < 0) {
      op <- flip_op(op)
    }
    f <- d$fun
    d <- new_term(coef, 0, function(env) f(env) / lead, linear_label(coef))
  } else {
    d$const <- 0
    d$label <- label
  }
  event_atom(d, op, value)
}

#' The comparison that holds when the sides are swapped (or negated).
#' @noRd
flip_op <- function(op) {
  switch(op,
    "<" = ">",
    "<=" = ">=",
    ">" = "<",
    ">=" = "<=",
    op
  )
}

#' A readable name for a linear quantity, such as `x - y`.
#' @noRd
linear_label <- function(coef) {
  parts <- vapply(seq_along(coef), function(i) {
    k <- coef[[i]]
    nm <- names(coef)[[i]]
    mag <- if (abs(k) == 1) nm else paste0(format(abs(k)), " * ", nm)
    sign <- if (k < 0) "- " else if (i > 1L) "+ " else ""
    paste0(sign, mag)
  }, character(1))
  paste(parts, collapse = " ")
}

#' @noRd
as_event <- function(x, arg) {
  if (inherits(x, "dst_event")) {
    return(x)
  }
  if (is.logical(x)) {
    return(event_const(x))
  }
  if (inherits(x, "dst_term")) {
    stop(
      "`", arg, "` must be a condition, such as `x > 0`, but `",
      x$label, "` is a quantity.",
      call. = FALSE
    )
  }
  stop("`", arg, "` must be a condition, such as `x > 0`.", call. = FALSE)
}

#' @export
Ops.dst_expr <- function(e1, e2) {
  op <- .Generic
  unary <- nargs() == 1L
  label <- if (unary) {
    paste0(op, expr_label(e1))
  } else {
    paste(expr_label(e1), op, expr_label(e2))
  }
  if (op %in% c("&", "|")) {
    return(event_node(
      if (op == "&") "and" else "or",
      list(as_event(e1, "event"), as_event(e2, "event"))
    ))
  }
  if (op == "!") {
    return(event_node("not", list(as_event(e1, "event"))))
  }
  if (inherits(e1, "dst_event") || (!unary && inherits(e2, "dst_event"))) {
    stop(
      "Conditions can only be combined with `&`, `|`, and `!`.",
      call. = FALSE
    )
  }
  if (unary) {
    if (op == "-") {
      return(term_scale(e1, -1, label))
    }
    return(e1)
  }
  t1 <- as_term(e1)
  t2 <- as_term(e2)
  if (op %in% c("==", "!=", "<", "<=", ">", ">=")) {
    return(term_compare(op, t1, t2, label))
  }
  term_arith(op, t1, t2, label)
}

#' @export
Math.dst_expr <- function(x, ...) {
  if (inherits(x, "dst_event")) {
    stop("`", .Generic, "()` cannot be applied to a condition.", call. = FALSE)
  }
  label <- paste0(.Generic, "(", x$label, ")")
  term_nonlinear(.Generic, list(x), label, args = list(...))
}

#' @noRd
expr_label <- function(x) {
  if (inherits(x, "dst_term")) {
    return(x$label)
  }
  if (inherits(x, "dst_event")) {
    return("<condition>")
  }
  format_value(x)
}

#' @export
print.dst_expr <- function(x, ...) {
  if (inherits(x, "dst_term")) {
    cat("<quantity: ", x$label, ">\n", sep = "")
  } else {
    cat("<condition>\n")
  }
  invisible(x)
}

# ---- recycling --------------------------------------------------------------

#' The values held by an event, in order.
#' @noRd
event_values <- function(ev) {
  switch(ev$type,
    atom = list(ev$value),
    const = list(ev$value),
    unlist(lapply(ev$children, event_values), recursive = FALSE)
  )
}

#' The common length of the values in some events.
#' @noRd
event_length <- function(events) {
  lens <- unlist(lapply(events, function(ev) {
    if (is.null(ev)) integer(0) else lengths(event_values(ev))
  }))
  n <- max(c(1L, lens))
  if (any(lens == 0L)) {
    n <- 0L
  }
  bad <- lens != 1L & lens != n
  if (any(bad)) {
    stop(
      "The values in the event have lengths ",
      paste(unique(lens), collapse = " and "), ".\n",
      "Values must have the same length, or length 1.",
      call. = FALSE
    )
  }
  n
}

#' Recycle an event's values to length `n`, or take rows of them.
#' @noRd
event_recycle <- function(ev, n, rows = NULL) {
  fix <- function(v) {
    if (length(v) == 1L) v <- rep(v, n)
    if (!is.null(rows)) v <- v[rows]
    v
  }
  if (ev$type %in% c("atom", "const")) {
    ev$value <- fix(ev$value)
    return(ev)
  }
  ev$children <- lapply(ev$children, event_recycle, n = n, rows = rows)
  ev
}

#' The variables an event mentions.
#' @noRd
event_variables <- function(ev, vars) {
  if (ev$type == "atom") {
    if (term_linear(ev$term)) {
      return(names(ev$term$coef))
    }
    # A non-linear quantity: find the variables it reads.
    probe <- stats::setNames(lapply(vars, function(v) 0), vars)
    used <- character(0)
    for (v in vars) {
      env <- probe
      env[[v]] <- NA_real_
      if (anyNA(suppressWarnings(ev$term$fun(env)))) {
        used <- c(used, v)
      }
    }
    return(used)
  }
  if (ev$type == "const") {
    return(character(0))
  }
  unique(unlist(lapply(ev$children, event_variables, vars = vars)))
}

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
  coords <- event_coordinates(clauses)
  if (length(coords) == 0L) {
    # No variables in it at all: the event holds or it does not.
    held <- Reduce(`|`, lapply(clauses, `[[`, "mask"))
    return(as.numeric(held))
  }
  dist_y <- coordinate_distribution(distribution, coords)
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

#' Probability of an event, given another.
#' @noRd
prob_given <- function(distribution, ev, gv, n) {
  vars <- variables(distribution)
  conj <- flatten_and(gv)
  # A quantity compared with `==` is conditioned on taking that value: a
  # variable, or a sum of multiples of them (a slice).
  is_value <- vapply(conj, function(a) {
    a$type == "atom" && a$op == "==" && term_linear(a$term)
  }, logical(1))
  is_unit <- vapply(conj[is_value], function(a) {
    length(a$term$coef) == 1L && a$term$coef[[1L]] == 1
  }, logical(1))
  fixed <- vapply(
    conj[is_value][is_unit],
    function(a) names(a$term$coef),
    ""
  )
  if (anyDuplicated(fixed)) {
    stop("`given` fixes the same variable twice.", call. = FALSE)
  }
  rest <- conj[!is_value]
  cond_event <- if (length(rest) == 0L) {
    NULL
  } else if (length(rest) == 1L) {
    rest[[1L]]
  } else {
    event_node("and", rest)
  }
  ratio <- function(d, rows) {
    e <- event_recycle(ev, n, rows)
    if (is.null(cond_event)) {
      return(prob_event(d, e))
    }
    g <- event_recycle(cond_event, n, rows)
    prob_event(d, event_node("and", list(e, g))) / prob_event(d, g)
  }
  if (!any(is_value)) {
    return(ratio(distribution, seq_len(n)))
  }
  mentioned <- event_variables(ev, vars)
  if (!is.null(cond_event)) {
    mentioned <- union(mentioned, event_variables(cond_event, vars))
  }
  clash <- intersect(mentioned, fixed)
  if (length(clash) > 0L) {
    stop(
      "The event mentions `", clash[[1L]], "`, which `given` fixes to a\n",
      "value. Leave it out of the event, or condition on it as an event\n",
      "(such as `", clash[[1L]], " >= a & ", clash[[1L]], " <= b`).",
      call. = FALSE
    )
  }
  if (length(fixed) == length(vars)) {
    stop(
      "`given` fixes every variable, which leaves nothing uncertain.",
      call. = FALSE
    )
  }
  # Fixing a combination of variables slices the distribution: append the
  # combination as a variable of its own, and condition on it.
  joint <- distribution
  idx <- match(fixed, vars)
  combos <- conj[is_value][!is_unit]
  if (length(combos) > 0L) {
    a <- diag(length(vars))
    rownames(a) <- vars
    colnames(a) <- vars
    for (k in seq_along(combos)) {
      row <- stats::setNames(numeric(length(vars)), vars)
      row[names(combos[[k]]$term$coef)] <- combos[[k]]$term$coef
      a <- rbind(a, row)
      rownames(a)[nrow(a)] <- paste0(".given", k)
    }
    joint <- eval_property(distribution, "linear", a)
    if (is.null(joint)) {
      stop(
        "Cannot condition on `", combos[[1L]]$term$label, "` taking a\n",
        "value, which combines several variables. This distribution\n",
        "does not state a `linear` property giving the distribution of\n",
        "such a combination.",
        call. = FALSE
      )
    }
    idx <- c(idx, length(vars) + seq_along(combos))
  }
  value_atoms <- c(conj[is_value][is_unit], combos)
  values <- as.data.frame(lapply(value_atoms, `[[`, "value"))
  groups <- vctrs::vec_group_id(values)
  out <- rep(NA_real_, n)
  for (grp in unique(groups)) {
    rows <- which(groups == grp)
    at <- unlist(values[rows[[1L]], , drop = TRUE], use.names = FALSE)
    if (anyNA(at)) {
      next
    }
    cond <- eval_property(joint, "conditional", idx, at)
    if (is.na(cond)) {
      out[rows] <- NaN
      next
    }
    variables(cond) <- setdiff(vars, fixed)
    out[rows] <- ratio(cond, rows)
  }
  out
}

#' The conditions joined by `&` at the top of an event.
#' @noRd
flatten_and <- function(ev) {
  if (ev$type == "and") {
    return(unlist(lapply(ev$children, flatten_and), recursive = FALSE))
  }
  list(ev)
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
