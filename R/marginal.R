#' Marginal Distribution
#'
#' The distribution of some of a multivariate distribution's variables,
#' ignoring the rest.
#'
#' @param distribution A distribution.
#' @param which The variables to keep: names, as in [variables()], or
#' positions. The result has them in this order.
#' @details
#' Selecting a single variable gives a univariate distribution, which can be
#' evaluated with [eval_cdf()], [eval_quantile()], and the rest.
#'
#' A distribution can state its own marginals (the multivariate Normal
#' does, as Normal distributions). Otherwise they are worked out: the CDF
#' is the joint CDF with the other variables at `Inf`; the survival
#' function is the joint one with the others at `-Inf`; the distribution of
#' finitely many points is found by adding up the points that share values;
#' and the density of a continuous distribution with one variable dropped is
#' found by integrating it out.
#' @returns A distribution: univariate if `which` selects one variable.
#' @examples
#' d <- dst_bi_norm(mean = c(0, 10), sd = c(1, 2), cor = 0.5)
#' marginal(d, "x2")
#' marginal(d, 1)
#'
#' e <- dst_mv_empirical(list(a = c(1, 1, 2), b = c(3, 4, 4)))
#' marginal(e, "b")
#' @export
marginal <- function(distribution, which) {
  checkmate::assert_class(distribution, "dst")
  idx <- resolve_variables(distribution, which, "which")
  if (length(idx) == 0) {
    stop("`which` must select at least one variable.")
  }
  p <- dimension(distribution)
  if (is.na(p) || p == 1L) {
    return(distribution)
  }
  if (identical(idx, seq_len(p))) {
    return(distribution)
  }
  eval_property(distribution, "marginal", idx)
}

#' Marginal distribution, for the property network.
#' @param idx Integer positions of the variables to keep.
#' @noRd
eval_mv_marginal_from_network <- function(distribution, idx) {
  s <- support(distribution)
  s_marg <- support_marginal(s, idx)
  p <- dimension(distribution)
  others <- setdiff(seq_len(p), idx)
  name <- paste("Marginal", pretty_name(distribution))
  # A finite distribution has a finite marginal: add up the points that share
  # the kept coordinates.
  pts <- enumerate_points(s)
  if (!is.null(pts)) {
    probs <- eval_joint(distribution, "pmf", as.list(pts))
    kept <- pts[idx]
    grp <- vctrs::vec_group_id(kept)
    probs <- as.numeric(tapply(probs, grp, sum))
    kept <- kept[!duplicated(grp), , drop = FALSE]
    if (length(idx) == 1L) {
      return(dst_finite(kept[[1L]], probs = probs / sum(probs)))
    }
    return(mv_finite(kept, probs / sum(probs), name = name))
  }
  # Fill in the dropped variables with a constant, so a joint representation
  # can be evaluated at the kept ones.
  fill <- function(args, value) {
    args <- vctrs::vec_recycle_common(!!!args)
    n <- length(args[[1L]])
    full <- vector("list", p)
    full[idx] <- args
    full[others] <- list(rep(value, n))
    full
  }
  reps <- list(
    cdf = function(...) eval_joint(distribution, "cdf", fill(list(...), Inf)),
    survival = function(...) {
      eval_joint(distribution, "survival", fill(list(...), -Inf))
    },
    realise = function(n) {
      draws <- realise(distribution, n = n)
      if (length(idx) == 1L) draws[[idx]] else draws[idx]
    }
  )
  if (vtype(distribution) == "continuous" && length(others) == 1L) {
    along <- regions(support_marginal(s, others))
    reps$density <- function(...) {
      args <- vctrs::vec_recycle_common(...)
      vapply(seq_along(args[[1L]]), function(i) {
        point <- lapply(args, function(v) v[[i]])
        if (anyNA(unlist(point))) {
          return(NA_real_)
        }
        integrand <- function(t) {
          full <- fill(lapply(point, rep_len, length(t)), 0)
          full[[others]] <- t
          eval_joint(distribution, "density", full)
        }
        integrate_regions(integrand, along)
      }, numeric(1))
    }
  }
  # Built here rather than by a user, so the warning about which
  # representations were left out is not for anyone. (`distribution` is the
  # argument here, so the constructor is fetched by mode.)
  build <- get("distribution", mode = "function")
  suppressWarnings(rlang::exec(
    build,
    !!!reps,
    .support = s_marg,
    .name = name
  ))
}
