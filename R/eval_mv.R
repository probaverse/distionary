#' Evaluate a Multivariate Distribution
#'
#' Evaluate the CDF, survival function, density, or PMF of a distribution of
#' several variables. The `eval_bi_*()` functions are for two variables,
#' taking one vector for each; the `eval_mv_*()` functions are for any number
#' of variables, taking a list of vectors, one per variable.
#'
#' @param distribution A distribution.
#' @param x,y For `eval_bi_*()`, vectors of values of the first and second
#' variables, in the order given by [variables()].
#' @param l For `eval_mv_*()`, a list of vectors, one for each variable. If
#' the list is named, each variable's vector is found by its name (see
#' [variables()]) and anything else in the list is ignored, so a data frame
#' with other columns, or the output of [realise()], can be passed as it
#' is. If the list is not named, it must have one vector per variable, in
#' order.
#' @param ... Not used; forces `known` to be named.
#' @param known Variables whose values are known, making the evaluation
#' conditional on them. Name them, as in [variables()], or give their
#' positions. In `eval_bi_*()`, `"x"` and `"y"` also refer to the arguments of
#' those names, unless they are the names of variables. See Details.
#' @details
#' ## The representations
#'
#' With variables \eqn{X_1, \ldots, X_p}, evaluated at
#' \eqn{x_1, \ldots, x_p}:
#'
#' - The CDF is \eqn{P(X_1 \le x_1, \ldots, X_p \le x_p)}.
#' - The survival function is \eqn{P(X_1 > x_1, \ldots, X_p > x_p)}: every
#'   variable exceeding its value. This is not one minus the CDF, which is
#'   the probability that *at least one* exceeds its value. The two agree
#'   only for a single variable. For the probability of any other event,
#'   such as `x <= 1, y > 3`, see [prob()].
#' - The density is the joint density, for a continuous distribution.
#' - The PMF is \eqn{P(X_1 = x_1, \ldots, X_p = x_p)}, for a discrete one.
#'
#' The vectors are recycled to a common length (in the manner of
#' [vctrs::vec_recycle_common()]), and the output has that length: element
#' `i` evaluates the representation at the point whose coordinates are the
#' `i`th element of each vector.
#'
#' ## Conditioning with `known`
#'
#' `known` names the variables whose values are known, and the
#' representation is then about the rest of them. Arguments stay in the same
#' order either way: you still pass a value for every variable, and `known`
#' says which of those values are known rather than evaluated at. For a
#' bivariate distribution of `x` and `y`:
#'
#' - `eval_bi_density(d, x, y, known = "x")` is the density of `y` at `y`,
#'   given that the first variable equals `x`.
#' - `eval_bi_cdf(d, x, y, known = "x")` is \eqn{P(Y \le y \mid X = x)}.
#' - `eval_bi_survival(d, x, y, known = "y")` is
#'   \eqn{P(X > x \mid Y = y)}.
#'
#' The variables in `known` are the ones held at their values, sitting on the
#' right of the bar in \eqn{P(\cdot \mid \cdot)}. The word was chosen because
#' that is how the bar is read aloud.
#'
#' Conditioning is on the variables *equalling* their values. To condition on
#' an event such as \eqn{X > x}, use [prob()]:
#' `prob(d, y > 3, given = x > 1)`.
#'
#' ## Which ones are available
#'
#' A distribution states what it can, and the rest is worked out from it.
#' The CDF and survival function are each worked out from the other. The PMF
#' of a continuous distribution is zero. A distribution on finitely many
#' points is worked out by listing the points.
#'
#' A conditional density or PMF is the joint one divided by that of the
#' `known` variables, which comes from [marginal()]. A conditional CDF or
#' survival function comes from the distribution's own `conditional`
#' property, if it states one (the multivariate Normal does); otherwise from
#' listing the points of a finite distribution; otherwise, for a continuous
#' distribution with a single variable left over, by integrating the
#' density.
#' @returns A numeric vector, with the common length of the inputs.
#' @seealso [prob()] for the probability of any event; [marginal()] for a
#' distribution of some of the variables.
#' @examples
#' d <- dst_bi_norm(mean = c(0, 0), sd = c(1, 1), cor = 0.6)
#' variables(d)
#' eval_bi_density(d, x = 0:2, y = 0)
#' eval_bi_cdf(d, x = 0, y = 0)
#' eval_bi_survival(d, x = 1, y = 1)
#'
#' # The same, with a list of vectors.
#' eval_mv_cdf(d, list(0, 0))
#' eval_mv_cdf(d, list(y = 0:1, x = 0))
#'
#' # The distribution of the second variable, when the first is known.
#' eval_bi_cdf(d, x = 2, y = 0:2, known = "x")
#' eval_bi_cdf(d, x = 2, y = 0:2, known = 1)
#' @name eval_mv
NULL

#' @rdname eval_mv
#' @export
eval_mv_cdf <- function(distribution, l, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_mv_representation(distribution, "cdf", l, known)
}

#' @rdname eval_mv
#' @export
eval_mv_survival <- function(distribution, l, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_mv_representation(distribution, "survival", l, known)
}

#' @rdname eval_mv
#' @export
eval_mv_density <- function(distribution, l, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_mv_representation(distribution, "density", l, known)
}

#' @rdname eval_mv
#' @export
eval_mv_pmf <- function(distribution, l, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_mv_representation(distribution, "pmf", l, known)
}

#' @rdname eval_mv
#' @export
eval_bi_cdf <- function(distribution, x, y, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_bi_representation(distribution, "cdf", x, y, known)
}

#' @rdname eval_mv
#' @export
eval_bi_survival <- function(distribution, x, y, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_bi_representation(distribution, "survival", x, y, known)
}

#' @rdname eval_mv
#' @export
eval_bi_density <- function(distribution, x, y, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_bi_representation(distribution, "density", x, y, known)
}

#' @rdname eval_mv
#' @export
eval_bi_pmf <- function(distribution, x, y, ..., known = NULL) {
  rlang::check_dots_empty()
  eval_bi_representation(distribution, "pmf", x, y, known)
}

# ---- internal ---------------------------------------------------------------

#' The bivariate evaluators: check there are two variables, then hand off.
#' @noRd
eval_bi_representation <- function(distribution, entry, x, y, known) {
  checkmate::assert_class(distribution, "dst")
  p <- dimension(distribution)
  if (!identical(p, 2L)) {
    fn <- paste0("eval_bi_", entry)
    stop(
      "`", fn, "()` is for distributions of two variables,\n",
      "and this one has ", format_dimension(p), ".\n",
      "Use `eval_mv_", entry, "()` for any number of variables."
    )
  }
  g <- resolve_variables(distribution, known, "known", aliases = c("x", "y"))
  eval_mv_representation(distribution, entry, list(x, y), g)
}

#' Evaluate a representation at the points given by a list of vectors,
#' possibly conditional on some of the variables.
#' @noRd
eval_mv_representation <- function(distribution, entry, l, known) {
  checkmate::assert_class(distribution, "dst")
  l <- as_eval_list(distribution, l)
  g <- resolve_variables(distribution, known, "known")
  if (length(g) == 0) {
    return(eval_joint(distribution, entry, l))
  }
  eval_conditional(distribution, entry, l, g)
}

#' Evaluate a representation at the points in an already-checked list.
#'
#' The one place that spreads a list of vectors into a representation's
#' arguments. A univariate distribution goes through its own evaluators.
#' @noRd
eval_joint <- function(distribution, entry, l) {
  if (!is_multivariate(distribution)) {
    return(eval_property(distribution, entry, l[[1L]]))
  }
  rlang::exec(eval_property, distribution, entry, !!!unname(l))
}

#' Check a list of vectors against a distribution's variables.
#'
#' Matches names to variables (reordering), checks there is one vector per
#' variable, and recycles them to a common length.
#' @returns The list, named by the variables and recycled.
#' @noRd
as_eval_list <- function(distribution, l, arg = "l") {
  p <- dimension(distribution)
  if (is.na(p)) {
    p <- 1L
  }
  if (!is.list(l)) {
    stop(
      "`", arg, "` must be a list of vectors, one per variable.\n",
      "A data frame works too."
    )
  }
  vars <- variables(distribution)
  if (is.null(vars)) {
    vars <- "x"
  }
  nms <- rlang::names2(l)
  if (length(l) > 0L && all(nms != "")) {
    # Named: take the variables by name, so that a data frame with other
    # columns can be passed as it is. A misspelled name leaves its variable
    # missing, which is an error, so nothing is silently skipped.
    missing <- setdiff(vars, nms)
    if (length(missing) > 0L) {
      stop(
        "`", arg, "` has no vector named `", missing[[1L]], "`.\n",
        "Name one vector after each variable: ", format_names(vars), "."
      )
    }
    l <- l[vars]
  } else if (any(nms != "")) {
    stop(
      "`", arg, "` has names on some vectors but not others.\n",
      "Name all of them, or none (to take them in order)."
    )
  } else if (length(l) != p) {
    stop(
      "`", arg, "` has ", length(l), " vectors, but the distribution has ",
      format_dimension(p), ".\n",
      "Give one vector per variable, or name them."
    )
  }
  l <- as.list(l)
  for (i in seq_along(l)) {
    if (!is.numeric(l[[i]]) && !all(is.na(l[[i]]))) {
      stop("Every vector in `", arg, "` must be numeric.")
    }
    l[[i]] <- as.numeric(l[[i]])
  }
  sizes <- lengths(l)
  size <- max(c(0L, sizes))
  if (any(sizes == 0L)) {
    size <- 0L
  }
  if (any(sizes != 1L & sizes != size)) {
    stop(
      "The vectors in `", arg, "` have lengths ",
      paste(unique(sizes), collapse = " and "), ".\n",
      "They must have the same length, or length 1."
    )
  }
  l <- lapply(l, function(v) if (length(v) == 1L) rep(v, size) else v)
  names(l) <- if (p == 1L) NULL else vars
  l
}

#' Turn a selection of variables into their positions.
#'
#' @param distribution The distribution whose variables are selected.
#' @param which Names (as in `variables()`) or positions, or `NULL` for none.
#' @param arg Name of the argument, for messages.
#' @param aliases Other names that refer to positions, used only for names
#' that are not variables: the argument names of `eval_bi_*()`.
#' @returns Integer positions, in the order given.
#' @noRd
resolve_variables <- function(distribution, which, arg, aliases = NULL) {
  if (is.null(which) || length(which) == 0) {
    return(integer(0))
  }
  p <- dimension(distribution)
  if (is.na(p)) {
    p <- 1L
  }
  vars <- variables(distribution)
  if (is.numeric(which)) {
    if (anyNA(which) || any(which != round(which)) ||
      any(which < 1) || any(which > p)) {
      stop(
        "`", arg, "` has a position outside 1 to ", p, ",\n",
        "the number of variables."
      )
    }
    idx <- as.integer(which)
  } else if (is.character(which)) {
    idx <- match(which, vars)
    if (!is.null(aliases)) {
      alias_idx <- match(which, aliases)
      idx[is.na(idx)] <- alias_idx[is.na(idx)]
    }
    if (anyNA(idx)) {
      bad <- which[is.na(idx)][[1L]]
      if (is.null(vars)) {
        stop(
          "`", arg, "` names a variable `", bad, "`, but a\n",
          "univariate distribution has no variable names."
        )
      }
      stop(
        "`", arg, "` names a variable `", bad, "` that the\n",
        "distribution does not have. Its variables are ",
        format_names(vars), "."
      )
    }
  } else {
    stop("`", arg, "` must be variable names or positions.")
  }
  if (anyDuplicated(idx)) {
    stop("`", arg, "` refers to the same variable twice.")
  }
  idx
}

#' Evaluate a representation of the non-given variables, conditional on the
#' given ones equalling their values.
#' @noRd
eval_conditional <- function(distribution, entry, l, g) {
  p <- length(l)
  r <- setdiff(seq_len(p), g)
  if (length(r) == 0) {
    stop(
      "Every variable is `known`, which leaves nothing to evaluate.\n",
      "Leave at least one variable out of `known`."
    )
  }
  if (entry %in% c("density", "pmf")) {
    joint <- eval_joint(distribution, entry, l)
    margin <- eval_joint(marginal(distribution, g), entry, l[g])
    return(joint / margin)
  }
  upper <- entry == "survival"
  cond <- distribution[["conditional"]]
  if (is.function(cond)) {
    return(conditional_by_property(distribution, cond, entry, l, g, r))
  }
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts)) {
    pmf <- eval_joint(distribution, "pmf", as.list(pts))
    return(conditional_by_points(pts, pmf, l, g, r, upper))
  }
  if (vtype(distribution) == "continuous" && length(r) == 1L) {
    return(conditional_by_integration(distribution, l, r, upper))
  }
  stop(
    "Cannot find this conditional ", entry, ".\n",
    "It can be worked out for a continuous distribution with one\n",
    "variable left over, or a finite one; otherwise, the distribution\n",
    "must state a `conditional` property."
  )
}

#' Conditional CDF or survival, from the distribution's `conditional`
#' property: one conditional distribution per distinct set of given values.
#' @noRd
conditional_by_property <- function(distribution, cond, entry, l, g, r) {
  n <- length(l[[1L]])
  out <- rep(NA_real_, n)
  given_vals <- as.data.frame(l[g])
  keys <- vctrs::vec_group_id(given_vals)
  for (k in unique(keys)) {
    rows <- which(keys == k)
    at <- unlist(given_vals[rows[[1L]], , drop = TRUE], use.names = FALSE)
    if (anyNA(at)) {
      next
    }
    d_cond <- cond(g, at)
    rest <- lapply(l[r], function(v) v[rows])
    out[rows] <- eval_joint(d_cond, entry, rest)
  }
  out
}

#' Conditional CDF or survival of a finite distribution, by listing its
#' points.
#' @param pts Data frame of the support's points; `pmf` their probabilities.
#' @param upper `TRUE` for the survival function (`>`), `FALSE` for the CDF.
#' @noRd
conditional_by_points <- function(pts, pmf, l, g, r, upper) {
  pts <- as.matrix(pts)
  q <- do.call(cbind, l)
  vapply(seq_len(nrow(q)), function(i) {
    if (anyNA(q[i, ])) {
      return(NA_real_)
    }
    on_given <- rowSums(pts[, g, drop = FALSE] ==
      rep(q[i, g], each = nrow(pts))) == length(g)
    qr <- rep(q[i, r], each = nrow(pts))
    in_rest <- if (upper) {
      pts[, r, drop = FALSE] > qr
    } else {
      pts[, r, drop = FALSE] <= qr
    }
    in_rest <- rowSums(in_rest) == length(r)
    sum(pmf[on_given & in_rest]) / sum(pmf[on_given])
  }, numeric(1))
}

#' Conditional CDF or survival of a continuous distribution with one variable
#' left over, by integrating the joint density along that variable.
#' @noRd
conditional_by_integration <- function(distribution, l, r, upper) {
  regions <- regions(support_marginal(support(distribution), r))
  n <- length(l[[1L]])
  vapply(seq_len(n), function(i) {
    point <- lapply(l, function(v) v[[i]])
    if (anyNA(unlist(point))) {
      return(NA_real_)
    }
    along <- function(t) {
      pt <- point
      pt[[r]] <- t
      pt <- lapply(pt, rep_len, length(t))
      eval_joint(distribution, "density", pt)
    }
    x <- point[[r]]
    total <- integrate_regions(along, regions)
    part <- if (upper) {
      integrate_regions(along, regions, from = x)
    } else {
      integrate_regions(along, regions, to = x)
    }
    part / total
  }, numeric(1))
}

#' Integrate a function over a set of regions, optionally clipped.
#' @param regions A two-column interval matrix.
#' @noRd
integrate_regions <- function(fun, regions, from = -Inf, to = Inf) {
  total <- 0
  for (j in seq_len(nrow(regions))) {
    lo <- max(regions[j, 1L], from)
    hi <- min(regions[j, 2L], to)
    if (lo < hi) {
      total <- total + distionary_integrate(fun, lo, hi)
    }
  }
  total
}

#' "1 variable", "2 variables".
#' @noRd
format_dimension <- function(p) {
  if (is.na(p)) {
    return("an unknown number of variables")
  }
  paste(p, if (p == 1L) "variable" else "variables")
}

#' `x`, `y`, and `z`.
#' @noRd
format_names <- function(vars) {
  vars <- paste0("`", vars, "`")
  if (length(vars) <= 2L) {
    return(paste(vars, collapse = " and "))
  }
  paste0(
    paste(vars[-length(vars)], collapse = ", "), ", and ", vars[length(vars)]
  )
}
