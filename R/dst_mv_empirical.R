#' Multivariate Empirical Distribution
#'
#' An empirical distribution of several variables, placing probability on
#' each observed point: each row of a data set, taken as a whole. By default
#' every observation gets equal probability; `weights` overrides that.
#' `dst_mv_empirical()` takes any number of variables; `dst_bi_empirical()`
#' is a shortcut for two.
#'
#' @param l For `dst_mv_empirical()`, a list of numeric vectors, one per
#' variable, all the same length (or length 1). A data frame is such a list.
#' Names, if any, name the variables.
#' @param x,y For `dst_bi_empirical()`, the observations of the two
#' variables (<[`data-masking`][rlang::args_data_masking]>). Each variable
#' is named after its expression when that is a bare column name or
#' variable, as in `dst_bi_empirical(flow, depth, data = df)`, and is
#' otherwise named `x` or `y`, after its argument.
#' @param ... Not used; forces the optional arguments to be named.
#' @param weights Weights for the observations, scaled to add up to 1.
#' For `dst_bi_empirical()`, data-masked like `x` and `y`.
#' @param data For `dst_bi_empirical()`, optionally a data frame in which to
#' find `x`, `y`, and `weights`.
#' @param na_action,na_action_w What to do with `NA`s in the observations
#' (an `NA` in any variable counts against the whole observation) or in the
#' weights: one of `"null"` (the default: return a Null distribution),
#' `"drop"`, or `"fail"`. As in [dst_empirical()], `"fail"` takes
#' precedence over `"null"`, which takes precedence over `"drop"`.
#' @details
#' Observations that repeat are combined into one point, with their weights
#' added. Its support is the set of distinct observed points (see
#' [discrete()]), and not every pairing of an observed value of one variable
#' with an observed value of another.
#'
#' A single variable gives a univariate empirical distribution
#' ([dst_empirical()]).
#' @returns A distribution with one variable per vector in `l` (or two, for
#' `dst_bi_empirical()`), or a Null distribution.
#' @examples
#' d <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(5, 4, 4, 6)))
#' d
#' eval_mv_pmf(d, list(a = 2, b = 4))
#' mean(d)
#' variance(d)
#'
#' df <- data.frame(flow = c(10, 12, 15, 11), depth = c(1, 1.5, 2, 1.1))
#' e <- dst_bi_empirical(flow, depth, data = df)
#' variables(e)
#' eval_bi_cdf(e, x = 12, y = 1.5)
#' @export
dst_mv_empirical <- function(
  l,
  ...,
  weights = 1,
  na_action = c("null", "drop", "fail"),
  na_action_w = c("null", "drop", "fail")
) {
  rlang::check_dots_empty()
  na_action <- rlang::arg_match(na_action)
  na_action_w <- rlang::arg_match(na_action_w)
  if (!is.list(l) || length(l) == 0) {
    stop(
      "`l` must be a list of vectors, one per variable.\n",
      "A data frame works too."
    )
  }
  if (length(l) == 1L) {
    return(dst_empirical(
      l[[1L]],
      weights = weights,
      na_action_y = na_action,
      na_action_w = na_action_w
    ))
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
      "another option for `na_action`."
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

#' @rdname dst_mv_empirical
#' @export
dst_bi_empirical <- function(
  x,
  y,
  ...,
  weights = 1,
  data = NULL,
  na_action = c("null", "drop", "fail"),
  na_action_w = c("null", "drop", "fail")
) {
  rlang::check_dots_empty()
  quo_x <- rlang::enquo(x)
  quo_y <- rlang::enquo(y)
  l <- list(
    rlang::eval_tidy(quo_x, data = data),
    rlang::eval_tidy(quo_y, data = data)
  )
  nms <- c(symbol_name(quo_x), symbol_name(quo_y))
  if (nms[[1L]] != "" && identical(nms[[1L]], nms[[2L]])) {
    nms <- c("", "")
  }
  names(l) <- bi_variable_names(nms)
  w <- rlang::eval_tidy(rlang::enquo(weights), data = data)
  dst_mv_empirical(
    l,
    weights = w,
    na_action = na_action,
    na_action_w = na_action_w
  )
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
  orthant_sum <- function(q, upper) {
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
    cdf = function(...) orthant_sum(as_matrix(...), upper = FALSE),
    survival = function(...) orthant_sum(as_matrix(...), upper = TRUE),
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
