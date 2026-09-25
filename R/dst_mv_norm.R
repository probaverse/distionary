#' Multivariate Normal Distribution
#'
#' Makes a multivariate Normal (Gaussian) distribution. `dst_mv_norm()` takes
#' any number of variables; `dst_bi_norm()` is a shortcut for two, specified
#' by standard deviations and a correlation. Either way, the result is the
#' same kind of distribution, with parameters `mean` and `cov`.
#'
#' @param mean Vector of means, one per variable. Its names, if any, name the
#' variables.
#' @param cov Covariance matrix: symmetric and positive semi-definite, with
#' one row and column per variable. Its dimnames, if any, name the
#' variables.
#' @param sd For `dst_bi_norm()`, the two standard deviations; positive.
#' @param cor For `dst_bi_norm()`, the correlation, strictly between -1 and
#' 1.
#' @details
#' Variables that are not named get the names `x1`, `x2`, and so on, except
#' in `dst_bi_norm()`, where they are `x` and `y` to match the arguments of
#' [eval_bi_cdf()] and the like.
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
#' ## Singular covariance
#'
#' A covariance matrix that is not positive definite (but is positive
#' semi-definite) describes a distribution on a lower-dimensional flat: a
#' line, a plane, and so on. Perfect correlation is one example; another
#' is a set of variables together with their sum, which is how a slice
#' such as \eqn{X + Y = s} is made (see the
#' "Multivariate Distributions" vignette). Such a distribution has
#' [vtype()] `"singular"`, a support built by [support_affine()], and no
#' density; its marginals and conditionals are again Normal. With no
#' variation at all, all probability sits at the mean.
#'
#' The CDF and survival function of a singular Normal are computed with
#' \pkg{mvtnorm}'s randomised algorithm, run with a fixed seed so that the
#' answer is always the same (your own random number stream is left as it
#' was). It is exact when the flat is a line, and accurate to about
#' \eqn{10^{-6}} otherwise.
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
  root <- cov_root(cov)
  if (root$rank == 0L) {
    # No variation at all: every variable sits at its mean.
    if (p == 1L) {
      out <- dst_degenerate(unname(mean))
      variables(out) <- vars
      return(out)
    }
    return(mv_finite(as.data.frame(as.list(mean)), 1, name = "Degenerate"))
  }
  if (p == 1L) {
    out <- dst_norm(mean = unname(mean), sd = sqrt(cov[[1L]]))
    variables(out) <- vars
    return(out)
  }
  if (root$rank == p) {
    chol_cov <- tryCatch(chol(cov), error = function(e) NULL)
    if (!is.null(chol_cov)) {
      return(new_mv_norm(mean, cov, chol_cov))
    }
  }
  new_mv_norm_singular(mean, cov, root$factor)
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
  # The bivariate shortcuts name their variables after the arguments of
  # `eval_bi_*()`, unless told otherwise.
  names(mean) <- bi_variable_names(names(mean))
  dst_mv_norm(mean = mean, cov = cov)
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
      z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
      x <- z %*% chol_cov + rep(mean, each = n)
      colnames(x) <- vars
      as.data.frame(x)
    },
    mean = mean,
    variance = cov,
    stdev = sqrt(diag(cov)),
    marginal = mv_norm_marginal(mean, cov),
    conditional = mv_norm_conditional(mean, cov),
    linear = function(matrix) {
      dst_mv_norm(
        mean = stats::setNames(as.numeric(matrix %*% mean), rownames(matrix)),
        cov = matrix %*% cov %*% t(matrix)
      )
    },
    .support = support,
    .name = if (p == 2L) "Bivariate Normal" else "Multivariate Normal"
  )
}

#' Build a singular multivariate Normal: `mean + factor %*% z`, for `z`
#' standard Normal of dimension `ncol(factor)`.
#' @noRd
new_mv_norm_singular <- function(mean, cov, factor) {
  p <- length(mean)
  r <- ncol(factor)
  vars <- names(mean)
  as_matrix <- function(...) {
    do.call(cbind, vctrs::vec_recycle_common(...))
  }
  base <- if (r == 1L) {
    continuous()
  } else {
    do.call(support_product, rep(list(continuous()), r))
  }
  distribution(
    .parameters = list(mean = mean, cov = cov),
    cdf = function(...) {
      x <- as_matrix(...)
      mvnorm_prob(lower = -Inf, upper = x, mean = mean, cov = cov)
    },
    survival = function(...) {
      x <- as_matrix(...)
      mvnorm_prob(lower = x, upper = Inf, mean = mean, cov = cov)
    },
    realise = function(n) {
      z <- matrix(stats::rnorm(n * r), nrow = n, ncol = r)
      x <- z %*% t(factor) + rep(mean, each = n)
      colnames(x) <- vars
      as.data.frame(x)
    },
    mean = mean,
    variance = cov,
    stdev = sqrt(diag(cov)),
    marginal = mv_norm_marginal(mean, cov),
    conditional = mv_norm_conditional(mean, cov),
    linear = function(matrix) {
      dst_mv_norm(
        mean = stats::setNames(as.numeric(matrix %*% mean), rownames(matrix)),
        cov = matrix %*% cov %*% t(matrix)
      )
    },
    .support = support_affine(base, shift = mean, matrix = factor),
    .name = if (p == 2L) "Bivariate Normal" else "Multivariate Normal"
  )
}

#' The `marginal` property of a multivariate Normal.
#' @noRd
mv_norm_marginal <- function(mean, cov) {
  function(which) {
    dst_mv_norm(mean = mean[which], cov = cov[which, which, drop = FALSE])
  }
}

#' The `conditional` property of a multivariate Normal.
#'
#' The usual formulas, with a pseudo-inverse in place of the inverse so that
#' the given variables may themselves have a singular covariance (as when
#' one of them is the sum of others). Conditioning on values the given
#' variables cannot take gives the Null distribution.
#' @noRd
mv_norm_conditional <- function(mean, cov) {
  p <- length(mean)
  vars <- names(mean)
  scale <- max(abs(diag(cov)))
  function(given, at) {
    rest <- setdiff(seq_len(p), given)
    s_rg <- cov[rest, given, drop = FALSE]
    s_gg <- cov[given, given, drop = FALSE]
    inv <- pinv_sym(s_gg, scale = scale)
    dev <- at - mean[given]
    # A value off the flat that the given variables live on has no chance
    # of being seen, and there is nothing to condition on.
    off <- dev - as.numeric(s_gg %*% inv %*% dev)
    if (any(abs(off) > 1e-8 * max(1, sqrt(scale), abs(dev)))) {
      return(dst_null())
    }
    weights <- s_rg %*% inv
    mu <- mean[rest] + as.numeric(weights %*% dev)
    names(mu) <- vars[rest]
    s <- cov[rest, rest, drop = FALSE] - weights %*% t(s_rg)
    s <- clean_cov((s + t(s)) / 2, scale = scale)
    dimnames(s) <- list(vars[rest], vars[rest])
    dst_mv_norm(mean = mu, cov = s)
  }
}

#' A square root of a covariance matrix, of the least rank.
#'
#' @param cov Symmetric matrix.
#' @param scale Size against which small eigenvalues count as zero;
#' defaults to the largest eigenvalue.
#' @returns A list with `rank` and `factor`, a `p` by `rank` matrix whose
#' product with its transpose is `cov`.
#' @noRd
cov_root <- function(cov, scale = NULL) {
  e <- eigen(cov, symmetric = TRUE)
  if (is.null(scale)) {
    scale <- max(abs(e$values), 0)
  }
  tol <- scale * nrow(cov) * sqrt(.Machine$double.eps)
  if (any(e$values < -tol)) {
    stop("`cov` must be positive semi-definite.")
  }
  keep <- e$values > tol
  factor <- e$vectors[, keep, drop = FALSE] %*%
    diag(sqrt(e$values[keep]), sum(keep))
  list(rank = sum(keep), factor = factor)
}

#' Rebuild a covariance matrix with its negligible eigenvalues set to zero.
#' @noRd
clean_cov <- function(cov, scale) {
  f <- cov_root(cov, scale = scale)$factor
  f %*% t(f)
}

#' Pseudo-inverse of a symmetric positive semi-definite matrix.
#' @noRd
pinv_sym <- function(m, scale) {
  e <- eigen(m, symmetric = TRUE)
  tol <- scale * nrow(m) * sqrt(.Machine$double.eps)
  keep <- e$values > tol
  v <- e$vectors[, keep, drop = FALSE]
  v %*% diag(1 / e$values[keep], sum(keep)) %*% t(v)
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
  singular <- cov_root(cov)$rank < p
  if (p == 2L && !singular) {
    sds <- sqrt(diag(cov))
    rho <- cov[1L, 2L] / prod(sds)
    if (abs(rho) < 0.925) {
      h <- (x[, 1L] - mean[[1L]]) / sds[[1L]]
      k <- (x[, 2L] - mean[[2L]]) / sds[[2L]]
      if (is.matrix(lower)) {
        return(pbinorm(-h, -k, rho))
      }
      return(pbinorm(h, k, rho))
    }
  }
  # The randomised algorithm is the only one that takes a singular
  # covariance, and is needed beyond 20 variables. It is run with a fixed
  # seed, so the same inputs always give the same answer.
  randomised <- singular || p > 20L
  algorithm <- if (randomised) {
    mvtnorm::GenzBretz(maxpts = 1e5, abseps = 1e-6, releps = 0)
  } else if (p <= 3L) {
    mvtnorm::TVPACK()
  } else {
    mvtnorm::Miwa()
  }
  one <- function(i) {
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
      algorithm = algorithm,
      # Fixes the randomised algorithm's seed, so the same inputs always give
      # the same answer; mvtnorm restores the caller's random numbers after.
      seed = if (randomised) 1L else NULL
    ))
  }
  vapply(seq_len(nrow(x)), one, numeric(1))
}

#' Standard bivariate Normal CDF, vectorised.
#'
#' \eqn{P(X \le h, Y \le k)} for standard Normal \eqn{X, Y} with
#' correlation `rho`, by the Sheppard--Drezner formula
#' \deqn{\Phi(h)\Phi(k) + \frac{1}{2\pi} \int_0^{\arcsin \rho}
#'   \exp\left(-\frac{h^2 + k^2 - 2hk\sin\theta}{2\cos^2\theta}
#'   \right) d\theta,}
#' with the integral done by 20-point Gauss--Legendre quadrature. This is
#' accurate to double precision for `abs(rho) < 0.925` (Genz, 2004), where
#' the integrand is smooth; the caller uses \pkg{mvtnorm} beyond that.
#' @param h,k Vectors of the same length.
#' @param rho Single correlation.
#' @noRd
pbinorm <- function(h, k, rho) {
  base <- stats::pnorm(h) * stats::pnorm(k)
  out <- base
  ok <- is.finite(h) & is.finite(k)
  if (rho != 0 && any(ok)) {
    gl <- gauss_legendre(20L)
    half <- asin(rho) / 2
    theta <- half * (gl$nodes + 1)
    sn <- sin(theta)
    hh <- h[ok]
    kk <- k[ok]
    expo <- outer(hh^2 + kk^2, rep(1, length(sn))) -
      2 * outer(hh * kk, sn)
    expo <- -expo / rep(2 * (1 - sn^2), each = length(hh))
    integral <- as.numeric(exp(expo) %*% gl$weights) * half
    out[ok] <- base[ok] + integral / (2 * pi)
  }
  # With an infinite limit, the probability is a univariate one (or 0).
  out[which(h == -Inf | k == -Inf)] <- 0
  only_k <- which(h == Inf & k > -Inf)
  out[only_k] <- stats::pnorm(k[only_k])
  only_h <- which(k == Inf & h > -Inf)
  out[only_h] <- stats::pnorm(h[only_h])
  out[is.na(h) | is.na(k)] <- NA_real_
  pmin(pmax(out, 0), 1)
}

#' Gauss-Legendre nodes and weights, from -1 to 1, by Golub-Welsch.
#' @noRd
gauss_legendre <- function(n) {
  i <- seq_len(n - 1L)
  off <- i / sqrt(4 * i^2 - 1)
  jacobi <- matrix(0, n, n)
  jacobi[cbind(i, i + 1L)] <- off
  jacobi[cbind(i + 1L, i)] <- off
  e <- eigen(jacobi, symmetric = TRUE)
  list(nodes = e$values, weights = 2 * e$vectors[1L, ]^2)
}
