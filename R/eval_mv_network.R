#' The property network for multivariate distributions.
#'
#' `eval_property()` routes a multivariate distribution here, to
#' `eval_mv_<entry>_from_network()`, when the distribution does not state the
#' property itself. Representations take one vector per variable, as the
#' stated ones do.
#' @noRd
NULL

#' @noRd
eval_mv_cdf_from_network <- function(distribution, ...) {
  l <- vctrs::vec_recycle_common(...)
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts) && has_stated(distribution, "pmf")) {
    return(prob_by_points(distribution, pts, l, rep(FALSE, length(l))))
  }
  if (has_stated(distribution, "survival")) {
    return(cdf_from_survival(distribution, l))
  }
  stop(
    "Cannot find the CDF. State a `cdf` or `survival`,\n",
    "or a `pmf` for a distribution on finitely many points."
  )
}

#' @noRd
eval_mv_survival_from_network <- function(distribution, ...) {
  l <- vctrs::vec_recycle_common(...)
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts) && has_stated(distribution, "pmf")) {
    return(prob_by_points(distribution, pts, l, rep(TRUE, length(l))))
  }
  if (has_stated(distribution, "cdf")) {
    return(prob_from_cdf(distribution, l, rep(TRUE, length(l))))
  }
  stop(
    "Cannot find the survival function. State a `cdf` or `survival`,\n",
    "or a `pmf` for a distribution on finitely many points."
  )
}

#' @noRd
eval_mv_pmf_from_network <- function(distribution, ...) {
  l <- vctrs::vec_recycle_common(...)
  # Neither kind has atoms: a singular distribution spreads its probability
  # over a lower-dimensional set, but still puts none on any single point.
  if (vtype(distribution) %in% c("continuous", "singular")) {
    out <- rep(0, length(l[[1L]]))
    out[is.na(Reduce(`+`, l))] <- NA_real_
    return(out)
  }
  stop("Cannot find the PMF, which must be stated in the distribution.")
}

#' @noRd
eval_mv_density_from_network <- function(distribution, ...) {
  if (vtype(distribution) == "singular") {
    stop(
      "This distribution has no density: its probability lies on a\n",
      "set spanning fewer dimensions than it has variables.\n",
      "Take a `marginal()` of fewer variables for one with a density."
    )
  }
  stop("Cannot find the density, which must be stated in the distribution.")
}

#' @noRd
eval_mv_realise_from_network <- function(distribution, n) {
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts)) {
    probs <- eval_joint(distribution, "pmf", as.list(pts))
    rows <- sample.int(nrow(pts), size = n, replace = TRUE, prob = probs)
    out <- pts[rows, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  stop(
    "Cannot draw from this distribution. State a `realise` function,\n",
    "or a `pmf` for a distribution on finitely many points."
  )
}

#' @noRd
eval_mv_realize_from_network <- function(distribution, n) {
  eval_property(distribution, "realise", n)
}

#' @noRd
eval_mv_mean_from_network <- function(distribution) {
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts)) {
    probs <- eval_joint(distribution, "pmf", as.list(pts))
    return(colSums(as.matrix(pts) * probs))
  }
  vars <- variables(distribution)
  out <- vapply(
    seq_along(vars),
    function(i) mean(marginal(distribution, i)),
    numeric(1)
  )
  names(out) <- vars
  out
}

#' The covariance matrix.
#' @noRd
eval_mv_variance_from_network <- function(distribution) {
  pts <- enumerate_points(support(distribution))
  if (!is.null(pts)) {
    probs <- eval_joint(distribution, "pmf", as.list(pts))
    return(weighted_cov(as.matrix(pts), probs))
  }
  stop(
    "Cannot find the covariance matrix, which must be stated as\n",
    "`variance` in the distribution."
  )
}

#' Standard deviation of each variable.
#' @noRd
eval_mv_stdev_from_network <- function(distribution) {
  if (has_stated(distribution, "variance")) {
    return(sqrt(diag(variance(distribution))))
  }
  vars <- variables(distribution)
  out <- vapply(
    seq_along(vars),
    function(i) stdev(marginal(distribution, i)),
    numeric(1)
  )
  names(out) <- vars
  out
}

# ---- helpers ----------------------------------------------------------------

#' Is a property stated in the distribution (rather than worked out)?
#' @noRd
has_stated <- function(distribution, entry) {
  !is.null(distribution[[entry]])
}

#' Covariance of the rows of a matrix under probabilities `probs`.
#' @noRd
weighted_cov <- function(x, probs) {
  mu <- colSums(x * probs)
  centred <- sweep(x, 2L, mu)
  crossprod(centred * sqrt(probs))
}

#' Probability from inequalities, by inclusion-exclusion on the CDF.
#'
#' The event is that each variable is at most its value (`upper` FALSE) or
#' exceeds it (`upper` TRUE). Expanding each "exceeds" as one minus "at most"
#' gives a signed sum of CDFs, with the variables not kept at their value
#' set to `Inf`.
#' @param l List of recycled vectors, one per variable.
#' @param upper Logical, one per variable.
#' @noRd
prob_from_cdf <- function(distribution, l, upper) {
  n <- length(l[[1L]])
  up <- which(upper)
  total <- rep(0, n)
  for (k in seq_len(2^length(up)) - 1L) {
    keep <- up[bitwAnd(k, as.integer(2^(seq_along(up) - 1L))) > 0]
    z <- l
    z[setdiff(up, keep)] <- list(rep(Inf, n))
    total <- total + (-1)^length(keep) * eval_joint(distribution, "cdf", z)
  }
  total
}

#' Probability from inequalities, by inclusion-exclusion on the survival
#' function.
#'
#' The mirror image of `prob_from_cdf()`: each "at most" is one minus
#' "exceeds", and variables not kept at their value are set to `-Inf`.
#' @noRd
prob_from_survival <- function(distribution, l, upper) {
  n <- length(l[[1L]])
  down <- which(!upper)
  total <- rep(0, n)
  for (k in seq_len(2^length(down)) - 1L) {
    keep <- down[bitwAnd(k, as.integer(2^(seq_along(down) - 1L))) > 0]
    z <- l
    z[setdiff(down, keep)] <- list(rep(-Inf, n))
    total <- total +
      (-1)^length(keep) * eval_joint(distribution, "survival", z)
  }
  total
}

#' The CDF, from the survival function.
#' @noRd
cdf_from_survival <- function(distribution, l) {
  prob_from_survival(distribution, l, rep(FALSE, length(l)))
}

#' Probability from inequalities for a finite distribution, by listing its
#' points.
#' @param upper Logical, one per variable: `>` if `TRUE`, `<=` if `FALSE`.
#' @param strict Logical, one per variable: whether to use `<` instead of
#' `<=`, or `>=` instead of `>`.
#' @noRd
prob_by_points <- function(
  distribution,
  pts,
  l,
  upper,
  strict = rep(FALSE, length(l))
) {
  probs <- eval_joint(distribution, "pmf", as.list(pts))
  pts <- as.matrix(pts)
  q <- do.call(cbind, l)
  vapply(seq_len(nrow(q)), function(i) {
    if (anyNA(q[i, ])) {
      return(NA_real_)
    }
    inside <- rep(TRUE, nrow(pts))
    for (j in seq_len(ncol(pts))) {
      v <- pts[, j]
      inside <- inside & if (upper[[j]]) {
        if (strict[[j]]) v >= q[i, j] else v > q[i, j]
      } else {
        if (strict[[j]]) v < q[i, j] else v <= q[i, j]
      }
    }
    sum(probs[inside])
  }, numeric(1))
}
