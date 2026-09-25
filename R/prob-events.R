# Events for `prob()`: what the variables are bound to while an event is
# evaluated, and what the comparisons and logic on them build.
#
# Each variable is bound to a *term*, a quantity computed from the
# variables. Arithmetic on terms makes new terms; comparing terms makes
# *events*, which `&`, `|`, and `!` combine. See `prob-evaluate.R` for how
# an event's probability is found.

# ---- terms: what a variable, or arithmetic on variables, evaluates to -----

#' A data mask binding each variable name to a term, for evaluating an
#' event.
#'
#' Each variable is an active binding: every time the expression reads it,
#' it gets a term with a fresh identity, and the identity is logged in
#' `log$read`. A term carries the identities it was built from, so once the
#' expression is evaluated, any identity read but missing from the result
#' was swallowed by something `prob()` cannot follow (such as `is.na()`),
#' and the answer would be wrong. See `check_event_reads()`.
#'
#' The mask also binds `%in%`, so that a variable can be compared with a
#' set of values.
#' @param log An environment holding `read`, an integer vector.
#' @noRd
event_mask <- function(vars, log) {
  bottom <- new.env(parent = emptyenv())
  for (v in vars) {
    local({
      name <- v
      makeActiveBinding(name, function() {
        id <- length(log$read) + 1L
        log$read <- c(log$read, id)
        variable_term(name, id)
      }, bottom)
    })
  }
  assign("%in%", term_in, envir = bottom)
  rlang::new_data_mask(bottom)
}

#' `%in%`, for a variable: equal to one of the values.
#' @noRd
term_in <- function(x, table) {
  if (inherits(table, "dst_expr")) {
    stop(
      "`%in%` needs a set of values on its right, not a variable.",
      call. = FALSE
    )
  }
  if (!inherits(x, "dst_term")) {
    return(base::`%in%`(x, table))
  }
  table <- unique(table)
  if (length(table) == 0L) {
    return(event_const(FALSE, ids = x$ids))
  }
  alts <- lapply(table, function(v) term_compare("==", x, as_term(v), "%in%"))
  if (length(alts) == 1L) {
    return(alts[[1L]])
  }
  event_node("or", alts)
}

#' A term: a quantity computed from the variables.
#'
#' A linear term is `sum(coef * variables) + const`, with `coef` a named
#' vector of single numbers. A non-linear term has `coef = NULL`, and is
#' known only through `fun`. Either way, `fun(env)` evaluates the part
#' other than `const` on a named list of variable values, and `const` may
#' be a vector (one value per probability asked for).
#' @noRd
new_term <- function(coef, const, fun, label, ids = integer(0)) {
  structure(
    list(coef = coef, const = const, fun = fun, label = label, ids = ids),
    class = c("dst_term", "dst_expr")
  )
}

#' @noRd
variable_term <- function(name, id = integer(0)) {
  new_term(
    coef = stats::setNames(1, name),
    const = 0,
    fun = function(env) env[[name]],
    label = name,
    ids = id
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
  ids <- union(t1$ids, t2$ids)
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
    return(new_term(coef, const, fun, label, ids))
  }
  new_term(NULL, const, fun, label, ids)
}

#' A term times a single number.
#' @noRd
term_scale <- function(t, k, label) {
  f <- t$fun
  coef <- if (term_linear(t)) t$coef * k else NULL
  if (!is.null(coef)) {
    coef <- coef[coef != 0]
  }
  new_term(coef, t$const * k, function(env) f(env) * k, label, t$ids)
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
  ids <- unique(unlist(lapply(terms, `[[`, "ids")))
  new_term(NULL, 0, fun, label, as.integer(ids))
}

#' Arithmetic on terms.
#' @noRd
term_arith <- function(op, t1, t2, label) {
  out <- term_arith_parts(op, t1, t2, label)
  # A side that has cancelled to a constant, such as `x - x`, still read
  # its variables.
  out$ids <- as.integer(union(union(t1$ids, t2$ids), out$ids))
  out
}

#' @noRd
term_arith_parts <- function(op, t1, t2, label) {
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
  new_event(list(
    type = "atom",
    term = term,
    op = op,
    value = value,
    ids = term$ids
  ))
}

#' @noRd
event_const <- function(value, ids = integer(0)) {
  new_event(list(type = "const", value = as.logical(value), ids = ids))
}

#' @noRd
event_node <- function(type, children) {
  ids <- unique(unlist(lapply(children, `[[`, "ids")))
  new_event(list(type = type, children = children, ids = as.integer(ids)))
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
    return(event_const(get(op)(d$const, 0), ids = d$ids))
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
    d <- new_term(
      coef,
      0,
      function(env) f(env) / lead,
      linear_label(coef),
      d$ids
    )
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
