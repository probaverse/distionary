#' Probability of an Orthant
#'
#' The probability that each variable falls on a chosen side of its value:
#' for example \eqn{P(X \le x, Y > y)}. These are the multivariate
#' counterparts of [prob_left()] and [prob_right()].
#'
#' @inheritParams eval_mv
#' @param ineq The inequality for each variable: `"<="`, `"<"`, `">"`, or
#' `">="`, read as "variable `ineq` value". A single one applies to every
#' variable; otherwise give one per variable, named to match
#' [variables()] or in order.
#' @details
#' An orthant is a region bounded on one side in each coordinate: the
#' multivariate version of a half-line. With every inequality `"<="`, its
#' probability is the CDF; with every one `">"`, the survival function.
#'
#' Strict and non-strict inequalities differ only where a variable has
#' atoms, and are treated alike for a variable that is continuous.
#'
#' The probability is worked out from the CDF (or the survival function) by
#' inclusion-exclusion, and for a distribution on finitely many points, by
#' adding up the points in the orthant.
#' @returns A numeric vector, with the common length of the inputs.
#' @seealso [eval_mv_cdf()], [eval_mv_survival()].
#' @examples
#' d <- dst_bi_norm(mean = c(0, 0), sd = c(1, 1), cor = 0.6)
#' # Both exceeding 1.
#' prob_bi_orthant(d, x = 1, y = 1, ineq = ">")
#' # The first at most 0, the second exceeding 0.
#' prob_bi_orthant(d, x = 0, y = 0, ineq = c("<=", ">"))
#'
#' e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
#' prob_mv_orthant(e, list(a = 2, b = 1), ineq = c(a = "<", b = "<="))
#' prob_mv_orthant(e, list(a = 2, b = 1), ineq = c(a = "<=", b = "<="))
#' @name orthant
NULL

#' @rdname orthant
#' @export
prob_mv_orthant <- function(distribution, l, ineq) {
  checkmate::assert_class(distribution, "dst")
  l <- as_eval_list(distribution, l)
  ineq <- as_ineq(distribution, ineq)
  orthant(distribution, l, ineq)
}

#' @rdname orthant
#' @export
prob_bi_orthant <- function(distribution, x, y, ineq) {
  checkmate::assert_class(distribution, "dst")
  p <- dimension(distribution)
  if (!identical(p, 2L)) {
    stop(
      "`prob_bi_orthant()` is for distributions of two variables,\n",
      "and this one has ", format_dimension(p), ".\n",
      "Use `prob_mv_orthant()` for any number of variables."
    )
  }
  prob_mv_orthant(distribution, list(x, y), ineq = unname(ineq))
}

# ---- internal ---------------------------------------------------------------

#' Check `ineq`, recycle it to one per variable, and put it in variable order.
#' @noRd
as_ineq <- function(distribution, ineq) {
  allowed <- c("<=", "<", ">", ">=")
  p <- dimension(distribution)
  if (is.na(p)) {
    p <- 1L
  }
  if (!is.character(ineq) || anyNA(ineq) || !all(ineq %in% allowed)) {
    stop(
      "Each entry of `ineq` must be one of ",
      "\"<=\", \"<\", \">\", or \">=\"."
    )
  }
  if (length(ineq) == 1L) {
    return(rep(unname(ineq), p))
  }
  if (length(ineq) != p) {
    stop(
      "`ineq` needs one inequality, or one per variable (",
      p, "), not ", length(ineq), "."
    )
  }
  nms <- rlang::names2(ineq)
  if (all(nms != "")) {
    idx <- resolve_variables(distribution, nms, "ineq")
    ineq[idx] <- ineq
  }
  unname(ineq)
}

#' The orthant probability, for checked inputs.
#' @noRd
orthant <- function(distribution, l, ineq) {
  if (!is_multivariate(distribution)) {
    upper <- ineq %in% c(">", ">=")
    inclusive <- ineq %in% c("<=", ">=")
    fn <- if (upper) prob_right else prob_left
    return(fn(distribution, of = l[[1L]], inclusive = inclusive))
  }
  upper <- ineq %in% c(">", ">=")
  strict <- ineq %in% c("<", ">=")
  s <- support(distribution)
  pts <- enumerate_points(s)
  if (!is.null(pts) && has_stated(distribution, "pmf")) {
    return(orthant_by_points(distribution, pts, l, upper, strict))
  }
  # A strict inequality differs from its non-strict one only by the atoms at
  # the value. Step to the atom below instead: `X < x` is `X <= x-`, and
  # `X >= x` is `X > x-`.
  for (j in which(strict)) {
    l[[j]] <- step_below(support_marginal(s, j), l[[j]])
  }
  if (all(upper) && has_stated(distribution, "survival")) {
    return(eval_joint(distribution, "survival", l))
  }
  if (!any(upper) || !has_stated(distribution, "survival")) {
    return(orthant_from_cdf(distribution, l, upper))
  }
  orthant_from_survival(distribution, l, upper)
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
