#' Marginal Distribution
#'
#' The distribution of some of a multivariate distribution's variables,
#' ignoring the rest.
#'
#' @param distribution A distribution.
#' @param which The variables to keep, in the order wanted: names, as in
#' [variables()], or positions. With \pkg{tidyselect} installed, any
#' tidyselect selection works too, such as bare names or
#' `c(runoff, everything())`.
#' @details
#' Selecting a single variable gives a univariate distribution, which can be
#' evaluated with [eval_cdf()], [eval_quantile()], and the rest.
#'
#' Like `dplyr::select()`, `marginal()` both picks variables and orders
#' them, so selecting all of them in a new order reorders the
#' distribution: `marginal(d, c("runoff", "rainfall"))`. Nothing is lost in
#' a reordering; every property the distribution states is kept, with its
#' arguments rearranged. The one thing a reordering cannot do is separate
#' variables whose support pairs them (such as the columns of a set of
#' points) with another variable in between.
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
#' marginal(d, "y")
#' marginal(d, 1)
#'
#' # Reorder the variables.
#' marginal(d, c("y", "x"))
#'
#' e <- dst_mv_empirical(list(a = c(1, 1, 2), b = c(3, 4, 4)))
#' marginal(e, "b")
#' @export
marginal <- function(distribution, which) {
  checkmate::assert_class(distribution, "dst")
  idx <- select_variables(distribution, rlang::enquo(which), "which")
  marginal_at(distribution, idx)
}

#' `marginal()`, for positions already worked out.
#' @noRd
marginal_at <- function(distribution, idx) {
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
  out <- eval_property(distribution, "marginal", idx)
  # The variables keep their names, whatever built the result.
  variables(out) <- variables(distribution)[idx]
  out
}

#' Marginal distribution, for the property network.
#' @param idx Integer positions of the variables to keep.
#' @noRd
eval_mv_marginal_from_network <- function(distribution, idx) {
  if (length(idx) == dimension(distribution)) {
    return(permute_distribution(distribution, idx))
  }
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

#' Reorder a distribution's variables, keeping every stated property.
#'
#' New variable `j` is old variable `idx[j]`. Representations are called
#' with their arguments put back in the old order; moments are rearranged;
#' and the `marginal`, `conditional`, and `linear` properties translate
#' positions between the two orders. Properties that are not stated are
#' left for the network to work out, as they would have been.
#' @noRd
permute_distribution <- function(distribution, idx) {
  back <- order(idx)
  old_vars <- variables(distribution)
  new_vars <- old_vars[idx]
  reps <- list()
  for (entry in c("cdf", "survival", "density", "pmf")) {
    f <- distribution[[entry]]
    if (is.function(f)) {
      reps[[entry]] <- permute_arguments(f, back)
    }
  }
  realise_old <- distribution[["realise"]]
  if (is.function(realise_old)) {
    reps$realise <- function(n) as.data.frame(realise_old(n))[idx]
  }
  if (!is.null(distribution[["mean"]])) {
    reps$mean <- distribution[["mean"]][idx]
  }
  if (!is.null(distribution[["stdev"]])) {
    reps$stdev <- distribution[["stdev"]][idx]
  }
  if (!is.null(distribution[["variance"]])) {
    reps$variance <- distribution[["variance"]][idx, idx, drop = FALSE]
  }
  marginal_old <- distribution[["marginal"]]
  if (is.function(marginal_old)) {
    reps$marginal <- function(which) marginal_old(idx[which])
  }
  conditional_old <- distribution[["conditional"]]
  if (is.function(conditional_old)) {
    reps$conditional <- function(given, at) {
      out <- conditional_old(idx[given], at)
      rest <- new_vars[-given]
      if (is.na(out) || length(rest) == 1L) {
        return(out)
      }
      variables(out) <- setdiff(old_vars, old_vars[idx[given]])
      marginal_at(out, match(rest, variables(out)))
    }
  }
  linear_old <- distribution[["linear"]]
  if (is.function(linear_old)) {
    reps$linear <- function(matrix) linear_old(matrix[, back, drop = FALSE])
  }
  build <- get("distribution", mode = "function")
  out <- suppressWarnings(rlang::exec(
    build,
    !!!reps,
    .support = support_marginal(support(distribution), idx),
    .name = pretty_name(distribution),
    .parameters = parameters(distribution)
  ))
  variables(out) <- new_vars
  out
}

#' A function of variables in the old order, called in the new order.
#' @noRd
permute_arguments <- function(f, back) {
  force(f)
  force(back)
  function(...) {
    args <- list(...)
    do.call(f, args[back])
  }
}
