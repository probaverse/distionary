#' Multivariate Normal Distribution
#'
#' Makes a multivariate Normal (Gaussian) distribution. `dst_mv_norm()` takes
#' any number of variables; `dst_bi_norm()` is a shortcut for two, specified
#' by standard deviations and a correlation.
#'
#' @param mean Vector of means, one per variable. Its names, if any, name the
#' variables.
#' @param cov Covariance matrix: symmetric and positive definite, with one
#' row and column per variable. Its dimnames, if any, name the variables.
#' @param sd For `dst_bi_norm()`, the two standard deviations; positive.
#' @param cor For `dst_bi_norm()`, the correlation, strictly between -1 and
#' 1.
#' @details
#' Variables that are not named get the names `x1`, `x2`, and so on.
#'
#' A single variable gives a univariate Normal distribution ([dst_norm()]).
#'
#' The marginal distribution of any of the variables (see [marginal()]) is
#' again Normal, as is their distribution conditional on the rest (see the
#' `given` argument of [eval_mv_cdf()]); both are stated exactly rather than
#' worked out.
#'
#' The CDF and survival function need the \pkg{mvtnorm} package. For up to
#' three variables they are computed to high accuracy; for more (up to 20),
#' with Miwa's algorithm, which is accurate but slows as the number of
#' variables grows.
#'
#' A covariance matrix that is not positive definite describes a
#' distribution on a lower-dimensional subspace (such as perfect
#' correlation, which places all probability on a line). These are not
#' supported yet.
#' @returns A distribution with one variable per entry of `mean`.
#' @examples
#' dst_mv_norm(mean = c(a = 0, b = 1, c = 2), cov = diag(3))
#' d <- dst_bi_norm(mean = c(0, 0), sd = c(1, 2), cor = 0.5)
#' d
#' variance(d)
#' eval_bi_density(d, x = 0, y = 0:2)
#' @export
dst_mv_norm <- function(mean, cov) {
  checkmate::assert_numeric(mean, min.len = 1)
  if (!is.matrix(cov)) {
    stop("`cov` must be a covariance matrix.")
  }
  checkmate::assert_numeric(cov)
  if (anyNA(mean) || anyNA(cov)) {
    return(dst_null())
  }
  p <- length(mean)
  if (!identical(dim(cov), c(p, p))) {
    stop(
      "`cov` must have one row and one column per mean (",
      p, " each)."
    )
  }
  vars <- mv_norm_names(mean, cov)
  names(mean) <- vars
  dimnames(cov) <- list(vars, vars)
  if (!isSymmetric(unname(cov))) {
    stop("`cov` must be symmetric.")
  }
  chol_cov <- tryCatch(chol(cov), error = function(e) NULL)
  if (is.null(chol_cov)) {
    stop(
      "`cov` must be positive definite.\n",
      "A singular covariance places the distribution on a lower-\n",
      "dimensional subspace, which is not supported yet."
    )
  }
  if (p == 1L) {
    return(dst_norm(mean = unname(mean), sd = sqrt(cov[[1L]])))
  }
  new_mv_norm(mean, cov, chol_cov)
}

#' @rdname dst_mv_norm
#' @export
dst_bi_norm <- function(mean, sd, cor) {
  checkmate::assert_numeric(mean, len = 2)
  checkmate::assert_numeric(sd, len = 2)
  checkmate::assert_number(cor, na.ok = TRUE)
  if (anyNA(mean) || anyNA(sd) || is.na(cor)) {
    return(dst_null())
  }
  if (any(sd <= 0)) {
    stop("`sd` must be positive.")
  }
  if (abs(cor) >= 1) {
    stop(
      "`cor` must be strictly between -1 and 1.\n",
      "A correlation of +/-1 places the distribution on a line, which\n",
      "is not supported yet."
    )
  }
  cov <- diag(sd) %*% matrix(c(1, cor, cor, 1), 2L) %*% diag(sd)
  d <- dst_mv_norm(mean = mean, cov = cov)
  parameters(d) <- list(mean = parameters(d)$mean, sd = unname(sd), cor = cor)
  d
}

#' Build a multivariate Normal from checked parameters.
#' @noRd
new_mv_norm <- function(mean, cov, chol_cov) {
  p <- length(mean)
  vars <- names(mean)
  log_det <- 2 * sum(log(diag(chol_cov)))
  as_matrix <- function(...) {
    args <- vctrs::vec_recycle_common(...)
    do.call(cbind, args)
  }
  support <- do.call(
    support_product,
    stats::setNames(rep(list(continuous()), p), vars)
  )
  distribution(
    .parameters = list(mean = mean, cov = cov),
    density = function(...) {
      x <- as_matrix(...)
      centred <- t(x) - mean
      z <- backsolve(chol_cov, centred, transpose = TRUE)
      quad <- colSums(z^2)
      exp(-(p * log(2 * pi) + log_det + quad) / 2)
    },
    cdf = function(...) {
      x <- as_matrix(...)
      mvnorm_prob(lower = -Inf, upper = x, mean = mean, cov = cov)
    },
    survival = function(...) {
      x <- as_matrix(...)
      mvnorm_prob(lower = x, upper = Inf, mean = mean, cov = cov)
    },
    realise = function(n) {
      z <- matrix(stats::rnorm(n * p), nrow = n)
      x <- z %*% chol_cov + rep(mean, each = n)
      colnames(x) <- vars
      as.data.frame(x)
    },
    mean = mean,
    variance = cov,
    stdev = sqrt(diag(cov)),
    marginal = function(which) {
      dst_mv_norm(mean = mean[which], cov = cov[which, which, drop = FALSE])
    },
    conditional = function(given, at) {
      rest <- setdiff(seq_len(p), given)
      s_rg <- cov[rest, given, drop = FALSE]
      s_gg <- cov[given, given, drop = FALSE]
      weights <- s_rg %*% solve(s_gg)
      mu <- mean[rest] + as.numeric(weights %*% (at - mean[given]))
      names(mu) <- vars[rest]
      s <- cov[rest, rest, drop = FALSE] - weights %*% t(s_rg)
      dst_mv_norm(mean = mu, cov = (s + t(s)) / 2)
    },
    .support = support,
    .name = if (p == 2L) "Bivariate Normal" else "Multivariate Normal"
  )
}

#' Variable names for a multivariate Normal, from `mean` or `cov`.
#' @noRd
mv_norm_names <- function(mean, cov) {
  from_mean <- names(mean)
  from_cov <- rownames(cov)
  if (is.null(from_cov)) {
    from_cov <- colnames(cov)
  }
  if (!is.null(from_mean) && !is.null(from_cov) &&
    !identical(from_mean, from_cov)) {
    stop("The names of `mean` and the dimnames of `cov` disagree.")
  }
  vars <- if (is.null(from_mean)) from_cov else from_mean
  if (is.null(vars)) {
    vars <- rep("", length(mean))
  }
  fill_variable_names(vars)
}

#' Probability that a multivariate Normal lies in a rectangle, row by row.
#'
#' One of `lower` and `upper` is a matrix with one row per rectangle; the
#' other is `-Inf` or `Inf`. The algorithm is chosen to be deterministic, so
#' the same inputs always give the same answer.
#' @noRd
mvnorm_prob <- function(lower, upper, mean, cov) {
  rlang::check_installed(
    "mvtnorm",
    reason = "to evaluate a multivariate Normal CDF or survival function."
  )
  p <- length(mean)
  x <- if (is.matrix(lower)) lower else upper
  algorithm <- if (p <= 3L) {
    mvtnorm::TVPACK()
  } else if (p <= 20L) {
    mvtnorm::Miwa()
  } else {
    mvtnorm::GenzBretz()
  }
  vapply(seq_len(nrow(x)), function(i) {
    row <- x[i, ]
    if (anyNA(row)) {
      return(NA_real_)
    }
    lo <- if (is.matrix(lower)) row else rep(-Inf, p)
    hi <- if (is.matrix(upper)) row else rep(Inf, p)
    # A coordinate pinned at an infinity it cannot exceed gives an empty
    # rectangle; mvtnorm needs this said rather than computed.
    if (any(lo == Inf) || any(hi == -Inf)) {
      return(0)
    }
    as.numeric(mvtnorm::pmvnorm(
      lower = lo,
      upper = hi,
      mean = unname(mean),
      sigma = unname(cov),
      algorithm = algorithm
    ))
  }, numeric(1))
}
