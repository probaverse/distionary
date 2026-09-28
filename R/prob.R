#' Probability of an Event
#'
#' The probability that a distribution's variables satisfy some conditions,
#' written the way you would write them for `dplyr::filter()`: `x < 2`,
#' `x <= 1, y > 3`, `x > 100 | y > 100`. Optionally, conditional on
#' another.
#'
#' @param distribution A distribution.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Conditions on
#' the distribution's variables, referred to by name (see [variables()]).
#' Several conditions must all hold, as in `filter()`: `x < 2, y > 3` is
#' the same event as `x < 2 & y > 3`. Within a condition, combine
#' comparisons with `&`, `|`, and `!`. A variable left out is
#' unrestricted, and no conditions at all is the certain event.
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
#' A condition looks like R code, but it is not run on data the way a
#' `filter()` condition is: `prob()` works out what region it describes.
#' So only these can be used on the variables:
#'
#' - arithmetic: `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`;
#' - comparisons: `<`, `<=`, `>`, `>=`, `==`, `!=`, and `%in%`;
#' - maths functions, such as `exp()`, `log()`, `abs()`, and `sqrt()`;
#' - logic: `&`, `|`, `!`, and `xor()`;
#' - functions of your own built only from these.
#'
#' Anything else applied to a variable, such as `is.na()`, `ifelse()`,
#' `pmax()`, or `&&`, is refused with an error naming it, rather than being
#' allowed to give a wrong answer. Values (not variables) can be computed
#' any way at all.
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
#' could not be found, rather than approximated. Answers are never
#' simulated: they come from the distribution's CDF (and the like), from
#' sums over its atoms, or from numerical integration, and are as accurate
#' as those.
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
#' prob(d, x < 0, y > 1)
#' prob(d, x > 2 | y > 5)
#' prob(d, 0 < x, x <= 1)
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
prob <- function(distribution, ..., given = NULL) {
  checkmate::assert_class(distribution, "dst")
  vars <- variables(distribution)
  if (is.null(vars)) {
    vars <- "x"
  }
  log <- new.env(parent = emptyenv())
  log$read <- integer(0)
  mask <- event_mask(vars, log)
  conditions <- rlang::enquos(...)
  named <- rlang::names2(conditions) != ""
  if (any(named)) {
    nm <- rlang::names2(conditions)[named][[1L]]
    stop(
      "`prob()` takes conditions, but `", nm, " = ...` is an argument.\n",
      "Did you mean `", nm, " == ...`?",
      call. = FALSE
    )
  }
  events <- lapply(conditions, eval_condition, mask = mask, log = log)
  # Several conditions must all hold, as in `filter()`.
  ev <- if (length(events) == 0L) {
    event_const(TRUE)
  } else if (length(events) == 1L) {
    events[[1L]]
  } else {
    event_node("and", events)
  }
  given <- rlang::enquo(given)
  gv <- if (rlang::quo_is_null(given)) {
    NULL
  } else {
    eval_condition(given, mask, log, arg = "given")
  }
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

#' Evaluate one condition, refusing anything `prob()` cannot follow.
#'
#' A variable read by the expression but missing from the result was
#' swallowed by something other than arithmetic, comparisons, maths
#' functions, `%in%`, and `&`, `|`, `!` (such as `is.na(x)`, which returns
#' plain logicals). Evaluating such an expression would give a wrong answer
#' without complaint, so it is refused.
#' @noRd
eval_condition <- function(quo, mask, log, arg = "event") {
  log$read <- integer(0)
  result <- tryCatch(
    rlang::eval_tidy(quo, data = mask),
    error = function(e) {
      odd <- unfollowable_calls(rlang::quo_get_expr(quo))
      if (length(odd) == 0L) {
        stop(e)
      }
      refuse_condition(quo, odd, conditionMessage(e))
    }
  )
  read <- log$read
  kept <- if (inherits(result, "dst_expr")) result$ids else integer(0)
  if (length(setdiff(read, kept)) > 0L) {
    refuse_condition(quo, unfollowable_calls(rlang::quo_get_expr(quo)))
  }
  as_event(result, arg)
}

#' Stop, explaining which parts of a condition `prob()` cannot follow.
#' @noRd
refuse_condition <- function(quo, odd, reason = NULL) {
  shown <- rlang::expr_deparse(rlang::quo_get_expr(quo), width = 60L)
  which <- if (length(odd) > 0L) {
    shown_odd <- ifelse(make.names(odd) == odd, paste0(odd, "()"), odd)
    paste0(
      "It uses ", paste0("`", shown_odd, "`", collapse = ", "),
      " on a variable, which it cannot follow."
    )
  } else {
    "It passes a variable through a function it cannot follow."
  }
  hint <- if (any(odd %in% c("&&", "||"))) {
    "Use `&` rather than `&&`, and `|` rather than `||`.\n"
  } else {
    ""
  }
  stop(
    "`prob()` cannot evaluate `", paste(shown, collapse = " "), "`.\n",
    which, "\n", hint,
    "Conditions can use arithmetic, comparisons, `%in%`, maths\n",
    "functions such as `exp()`, and `&`, `|`, `!`; see `?prob`.",
    if (!is.null(reason)) paste0("\n(It failed with: ", reason, ")"),
    call. = FALSE
  )
}

#' Functions called on something mentioning a variable, other than the ones
#' `prob()` understands.
#' @noRd
unfollowable_calls <- function(expr) {
  understood <- c(
    "+", "-", "*", "/", "^", "%%", "%/%",
    "==", "!=", "<", "<=", ">", ">=", "&", "|", "!", "(", "%in%", "xor",
    "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif",
    "exp", "log", "expm1", "log1p", "log2", "log10",
    "cos", "sin", "tan", "cospi", "sinpi", "tanpi",
    "acos", "asin", "atan", "cosh", "sinh", "tanh",
    "acosh", "asinh", "atanh", "lgamma", "gamma", "digamma", "trigamma"
  )
  found <- character(0)
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible())
    }
    fn <- e[[1L]]
    name <- if (is.symbol(fn)) as.character(fn) else ""
    if (!name %in% understood && name != "" && !identical(name, "$")) {
      found <<- c(found, name)
    }
    for (arg in as.list(e)[-1L]) {
      walk(arg)
    }
  }
  walk(expr)
  unique(setdiff(found, c("c", ".env", ".data")))
}