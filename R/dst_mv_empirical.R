#' Multivariate Empirical Distribution
#'
#' An empirical distribution of several variables, placing probability on
#' each observed point: each row of a data set, taken as a whole. By default
#' every observation gets equal probability; `weights` overrides that.
#' `dst_mv_empirical()` takes any number of variables; `dst_bi_empirical()`
#' is a shortcut for two.
#'
#' @param ... For `dst_mv_empirical()`,
#' <[`data-masking`][rlang::args_data_masking]> the observations of each
#' variable: numeric vectors of the same length (or length 1), or columns of
#' `data`. A list or data frame of such vectors can also go here, and
#' stands for its vectors, so `dst_mv_empirical(df)` takes every column of
#' `df`, and `dst_mv_empirical(df[cols])` some of them. Not used in
#' `dst_bi_empirical()`, where it forces the other arguments to be named.
#' @param x,y For `dst_bi_empirical()`, the observations of the two
#' variables (<[`data-masking`][rlang::args_data_masking]>).
#' @param weights <[`data-masking`][rlang::args_data_masking]> Weights for
#' the observations, scaled to add up to 1.
#' @param data Optionally, a data frame in which to find the variables and
#' `weights`.
#' @param na_action_y,na_action_w What to do with `NA`s in the observations
#' (an `NA` in any variable counts against the whole observation) or in the
#' weights: one of `"null"` (the default: return a Null distribution),
#' `"drop"`, or `"fail"`. As in [dst_empirical()], `"fail"` takes
#' precedence over `"null"`, which takes precedence over `"drop"`.
#' @details
#' A variable is named by its argument name (`flow = x1`); failing that,
#' after its expression when that is a bare name (`flow`); failing that,
#' `x1`, `x2`, and so on, by position (`x` and `y` in `dst_bi_empirical()`).
#' A list's vectors are named by their names in the list.
#'
#' Observations that repeat are combined into one point, with their weights
#' added. Its support is the set of distinct observed points (see
#' [discrete()]), and not every pairing of an observed value of one variable
#' with an observed value of another.
#'
#' A single variable gives a univariate empirical distribution
#' ([dst_empirical()]).
#' @returns A distribution with one variable per vector given (or two, for
#' `dst_bi_empirical()`), or a Null distribution.
#' @examples
#' d <- dst_mv_empirical(a = c(1, 2, 2, 3), b = c(5, 4, 4, 6))
#' d
#' eval_mv_pmf(d, list(a = 2, b = 4))
#' mean(d)
#' variance(d)
#'
#' df <- data.frame(
#'   site = c("a", "a", "b", "b"),
#'   flow = c(10, 12, 15, 11),
#'   depth = c(1, 1.5, 2, 1.1)
#' )
#' dst_mv_empirical(flow, depth, data = df)
#' dst_mv_empirical(df[c("flow", "depth")])
#' e <- dst_bi_empirical(flow, depth, data = df)
#' eval_bi_cdf(e, x = 12, y = 1.5)
#' @export
dst_mv_empirical <- function(
  ...,
  weights = 1,
  data = NULL,
  na_action_y = c("null", "drop", "fail"),
  na_action_w = c("null", "drop", "fail")
) {
  cols <- empirical_columns(rlang::enquos(...), data)
  if (length(cols) == 0L) {
    stop("Give the observations of at least one variable.")
  }
  names(cols) <- fill_variable_names(names(cols))
  w <- rlang::eval_tidy(rlang::enquo(weights), data = data)
  empirical_from_columns(cols, w, na_action_y, na_action_w)
}

#' @rdname dst_mv_empirical
#' @export
dst_bi_empirical <- function(
  x,
  y,
  ...,
  weights = 1,
  data = NULL,
  na_action_y = c("null", "drop", "fail"),
  na_action_w = c("null", "drop", "fail")
) {
  rlang::check_dots_empty()
  cols <- empirical_columns(
    list(x = rlang::enquo(x), y = rlang::enquo(y)),
    data,
    arg_names = FALSE
  )
  if (length(cols) != 2L) {
    stop("`x` and `y` must each be one variable's observations.")
  }
  nms <- names(cols)
  if (nms[[1L]] != "" && identical(nms[[1L]], nms[[2L]])) {
    nms <- c("", "")
  }
  names(cols) <- bi_variable_names(nms)
  w <- rlang::eval_tidy(rlang::enquo(weights), data = data)
  empirical_from_columns(cols, w, na_action_y, na_action_w)
}

#' Evaluate the observation arguments, splicing lists, and name them.
#'
#' @param quos Quosures, one per argument.
#' @param arg_names Whether an argument's name names its variable (in
#' `dst_bi_empirical()`, the names are just `x` and `y`).
#' @returns A list of vectors, named where a name was found, `""`
#' otherwise.
#' @noRd
empirical_columns <- function(quos, data, arg_names = TRUE) {
  out <- list()
  nms <- character(0)
  given <- rlang::names2(quos)
  for (k in seq_along(quos)) {
    value <- rlang::eval_tidy(quos[[k]], data = data)
    if (is.list(value)) {
      if (arg_names && given[[k]] != "") {
        stop(
          "Argument `", given[[k]], "` is a list of variables, which\n",
          "cannot take one name. Name the vectors inside it instead."
        )
      }
      out <- c(out, unname(as.list(value)))
      nms <- c(nms, rlang::names2(value))
      next
    }
    out <- c(out, list(value))
    nms <- c(nms, if (arg_names && given[[k]] != "") {
      given[[k]]
    } else {
      symbol_name(quos[[k]])
    })
  }
  names(out) <- nms
  out
}

#' An empirical distribution from named columns of observations.
#' @noRd
empirical_from_columns <- function(l, weights, na_action_y, na_action_w) {
  na_action <- rlang::arg_match(na_action_y, c("null", "drop", "fail"))
  na_action_w <- rlang::arg_match(na_action_w, c("null", "drop", "fail"))
  if (length(l) == 1L) {
    out <- dst_empirical(
      l[[1L]],
      weights = weights,
      na_action_y = na_action,
      na_action_w = na_action_w
    )
    if (!is.na(out)) {
      variables(out) <- names(l)
    }
    return(out)
  }
  vars <- fill_variable_names(rlang::names2(l))
  for (v in l) {
    checkmate::assert_numeric(v)
  }
  checkmate::assert_numeric(weights)
  cols <- vctrs::vec_recycle_common(!!!unname(as.list(l)), weights)
  w <- cols[[length(cols)]]
  pts <- as.data.frame(cols[-length(cols)], col.names = vars)
  names(pts) <- vars
  na_pts <- !stats::complete.cases(pts)
  na_w <- is.na(w)
  if (any(na_w) && na_action_w == "fail") {
    stop(
      "Weights have NA values. Deal with these, or choose\n",
      "another option for `na_action_w`."
    )
  }
  if (any(na_pts) && na_action == "fail") {
    stop(
      "Observations have NA values. Deal with these, or choose\n",
      "another option for `na_action_y`."
    )
  }
  if ((any(na_w) && na_action_w == "null") ||
    (any(na_pts) && na_action == "null")) {
    return(dst_null())
  }
  keep <- !na_pts & !na_w
  pts <- pts[keep, , drop = FALSE]
  w <- w[keep]
  if (any(w < 0)) {
    stop("Weights must not be negative.")
  }
  if (nrow(pts) == 0 || sum(w) == 0) {
    warning(
      "Can't make an empirical distribution from no data.\n",
      "Returning a Null distribution."
    )
    return(dst_null())
  }
  grp <- vctrs::vec_group_id(pts)
  probs <- as.numeric(tapply(w, grp, sum))
  pts <- pts[!duplicated(grp), , drop = FALSE]
  kept <- probs > 0
  pts <- pts[kept, , drop = FALSE]
  probs <- probs[kept] / sum(probs[kept])
  rownames(pts) <- NULL
  mv_finite(pts, probs)
}

#' A distribution on finitely many points.
#'
#' @param pts Data frame of distinct points, one column per variable.
#' @param probs Their probabilities, positive and adding up to 1.
#' @param name Name for the distribution.
#' @noRd
mv_finite <- function(pts, probs, name = NULL) {
  p <- ncol(pts)
  vars <- names(pts)
  mat <- as.matrix(pts)
  if (is.null(name)) {
    name <- if (p == 2L) "Bivariate Finite" else "Multivariate Finite"
  }
  as_matrix <- function(...) {
    do.call(cbind, vctrs::vec_recycle_common(...))
  }
  prob_sum <- function(q, upper) {
    vapply(seq_len(nrow(q)), function(i) {
      if (anyNA(q[i, ])) {
        return(NA_real_)
      }
      qi <- rep(q[i, ], each = nrow(mat))
      inside <- if (upper) mat > qi else mat <= qi
      sum(probs[rowSums(inside) == p])
    }, numeric(1))
  }
  mu <- colSums(mat * probs)
  distribution(
    .parameters = list(outcomes = pts, probs = probs),
    pmf = function(...) {
      q <- as.data.frame(vctrs::vec_recycle_common(...))
      names(q) <- vars
      matched <- vctrs::vec_match(q, pts)
      out <- probs[matched]
      out[is.na(matched)] <- 0
      out[!stats::complete.cases(q)] <- NA_real_
      out
    },
    cdf = function(...) prob_sum(as_matrix(...), upper = FALSE),
    survival = function(...) prob_sum(as_matrix(...), upper = TRUE),
    realise = function(n) {
      rows <- sample.int(nrow(pts), size = n, replace = TRUE, prob = probs)
      out <- pts[rows, , drop = FALSE]
      rownames(out) <- NULL
      out
    },
    mean = mu,
    variance = weighted_cov(mat, probs),
    .support = discrete(pts),
    .name = name
  )
}

#' The name of a quosure's expression, if it is a bare symbol; else "".
#' @noRd
symbol_name <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr)) rlang::as_name(expr) else ""
}
