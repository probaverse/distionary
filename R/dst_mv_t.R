#' Multivariate Student t Distribution
#'
#' Makes a multivariate Student t distribution. `dst_mv_t()` takes any
#' number of variables; `dst_bi_t()` is a shortcut for two, specified by
#' scales and a correlation.
#'
#' @param location Vector of locations, one per variable. Its names, if any,
#' name the variables.
#' @param scale For `dst_mv_t()`, the scale matrix: symmetric and positive
#' semi-definite, with one row and column per variable (its dimnames, if
#' any, name the variables). For `dst_bi_t()`, the two scales; positive.
#' @param df Degrees of freedom; single positive number, not necessarily a
#' whole one. `Inf` gives the multivariate Normal.
#' @param cor For `dst_bi_t()`, the correlation parameter, strictly between
#' -1 and 1.
#' @details
#' The distribution is that of \eqn{\mu + Z / \sqrt{W / \nu}}, where
#' \eqn{Z} is multivariate Normal with mean zero and covariance `scale`,
#' \eqn{W} is chi-squared with \eqn{\nu} = `df` degrees of freedom, and
#' \eqn{\mu} = `location`. Its tails are heavier than the Normal's, and its
#' variables are dependent even when uncorrelated: an extreme \eqn{W}
#' pushes all of them out together.
#'
#' The `scale` matrix is not the covariance: that is
#' `scale * df / (df - 2)`, for `df > 2`. Likewise, `location` is the mean
#' only for `df > 1`.
#'
#' Variables that are not named get the names `x1`, `x2`, and so on, except
#' in `dst_bi_t()`, where they are `x` and `y`.
#'
#' The marginal distribution of any of the variables (see [marginal()]) is
#' again t with the same degrees of freedom; one variable gives [dst_t()].
#' Given some of the variables (see [conditional()]), the rest are t with
#' more degrees of freedom: `df` plus the number of variables given. Both
#' are exact.
#'
#' ## Singular scale
#'
#' As with [dst_mv_norm()], a scale matrix that is only positive
#' semi-definite gives a distribution on a lower-dimensional flat, with
#' [vtype()] `"singular"` and no density. This is what slicing needs.
#'
#' ## Computing the CDF
#'
#' The CDF and survival function average multivariate Normal probabilities
#' over \eqn{W}, which is a one-dimensional integral. This allows any `df`
#' (\pkg{mvtnorm}'s own t probabilities need a whole number), and needs
#' \pkg{mvtnorm} for the Normal probabilities.
#' @returns A distribution with one variable per entry of `location`.
#' @examples
#' dst_mv_t(
#'   location = c(a = 0, b = 1, c = 2),
#'   scale = diag(3),
#'   df = 4.5
#' )
#' d <- dst_bi_t(location = c(0, 0), scale = c(1, 2), cor = 0.5, df = 3)
#' d
#' variance(d)
#' eval_bi_density(d, x = 0, y = 0:2)
#' marginal(d, "y")
#' @export
dst_mv_t <- function(location, scale, df) {
  checkmate::assert_numeric(location, min.len = 1)
  if (!is.matrix(scale)) {
    stop("`scale` must be a scale matrix.")
  }
  checkmate::assert_numeric(scale)
  checkmate::assert_number(df, na.ok = TRUE)
  if (anyNA(location) || anyNA(scale) || is.na(df)) {
    return(dst_null())
  }
  if (df <= 0) {
    stop("`df` must be positive.")
  }
  p <- length(location)
  if (!identical(dim(scale), c(p, p))) {
    stop(
      "`scale` must have one row and one column per location (",
      p, " each)."
    )
  }
  if (is.infinite(df)) {
    return(dst_mv_norm(mean = location, cov = scale))
  }
  vars <- mv_norm_names(location, scale)
  names(location) <- vars
  dimnames(scale) <- list(vars, vars)
  if (!isSymmetric(unname(scale))) {
    stop("`scale` must be symmetric.")
  }
  root <- cov_root(scale)
  if (root$rank == 0L) {
    if (p == 1L) {
      return(dst_degenerate(unname(location)))
    }
    return(mv_finite(
      as.data.frame(as.list(location)),
      1,
      name = "Degenerate"
    ))
  }
  if (p == 1L) {
    return(dst_t(df, location = unname(location), scale = sqrt(scale[[1L]])))
  }
  new_mv_t(location, scale, df, root)
}

#' @rdname dst_mv_t
#' @export
dst_bi_t <- function(location, scale, cor, df) {
  checkmate::assert_numeric(location, len = 2)
  checkmate::assert_numeric(scale, len = 2)
  checkmate::assert_number(cor, na.ok = TRUE)
  checkmate::assert_number(df, na.ok = TRUE)
  if (anyNA(location) || anyNA(scale) || is.na(cor) || is.na(df)) {
    return(dst_null())
  }
  if (any(scale <= 0)) {
    stop("`scale` must be positive.")
  }
  if (abs(cor) >= 1) {
    stop(
      "`cor` must be strictly between -1 and 1.\n",
      "A correlation of +/-1 places the distribution on a line;\n",
      "for that, give `dst_mv_t()` a singular `scale` matrix."
    )
  }
  scale_matrix <- diag(scale) %*% matrix(c(1, cor, cor, 1), 2L) %*%
    diag(scale)
  names(location) <- bi_variable_names(names(location))
  d <- dst_mv_t(location = location, scale = scale_matrix, df = df)
  parameters(d) <- list(
    location = parameters(d)$location,
    scale = unname(scale),
    cor = cor,
    df = df
  )
  d
}

#' Build a multivariate t from checked parameters.
#' @param root From `cov_root(scale)`: its rank and a square-root factor.
#' @noRd
new_mv_t <- function(location, scale, df, root) {
  p <- length(location)
  vars <- names(location)
  factor <- root$factor
  r <- root$rank
  as_matrix <- function(...) {
    do.call(cbind, vctrs::vec_recycle_common(...))
  }
  reps <- list(
    cdf = function(...) {
      mvt_prob(as_matrix(...), upper = TRUE, location, scale, df)
    },
    survival = function(...) {
      mvt_prob(as_matrix(...), upper = FALSE, location, scale, df)
    },
    realise = function(n) {
      z <- matrix(stats::rnorm(n * r), nrow = n) %*% t(factor)
      w <- stats::rchisq(n, df = df)
      x <- z / sqrt(w / df) + rep(location, each = n)
      colnames(x) <- vars
      as.data.frame(x)
    },
    mean = stats::setNames(
      if (df > 1) unname(location) else rep(NaN, p),
      vars
    ),
    variance = mv_t_covariance(scale, df),
    stdev = sqrt(diag(mv_t_covariance(scale, df))),
    marginal = function(which) {
      dst_mv_t(
        location = location[which],
        scale = scale[which, which, drop = FALSE],
        df = df
      )
    },
    conditional = mv_t_conditional(location, scale, df)
  )
  if (r == p) {
    chol_scale <- chol(scale)
    log_det <- 2 * sum(log(diag(chol_scale)))
    const <- lgamma((df + p) / 2) - lgamma(df / 2) -
      p / 2 * log(df * pi) - log_det / 2
    reps$density <- function(...) {
      x <- as_matrix(...)
      z <- backsolve(chol_scale, t(x) - location, transpose = TRUE)
      quad <- colSums(z^2)
      exp(const - (df + p) / 2 * log1p(quad / df))
    }
    support <- do.call(
      support_product,
      stats::setNames(rep(list(continuous()), p), vars)
    )
  } else {
    base <- if (r == 1L) {
      continuous()
    } else {
      do.call(support_product, rep(list(continuous()), r))
    }
    support <- support_affine(base, shift = location, matrix = factor)
  }
  build <- get("distribution", mode = "function")
  rlang::exec(
    build,
    !!!reps,
    .parameters = list(location = location, scale = scale, df = df),
    .support = support,
    .name = if (p == 2L) "Bivariate Student t" else "Multivariate Student t"
  )
}

#' Covariance matrix of a multivariate t, where it exists.
#'
#' Past `df > 2`, a multiple of the scale. For `1 < df <= 2`, the
#' variances are infinite and the covariances do not exist (`NaN`); for
#' `df <= 1`, nothing does.
#' @noRd
mv_t_covariance <- function(scale, df) {
  if (df > 2) {
    return(scale * df / (df - 2))
  }
  out <- scale
  out[] <- NaN
  if (df > 1) {
    diag(out)[diag(scale) > 0] <- Inf
    diag(out)[diag(scale) == 0] <- 0
  }
  out
}

#' The `conditional` property of a multivariate t.
#'
#' Given the variables `g` at `a`, the rest are t with `df + k` degrees of
#' freedom, where `k` is the number of dimensions the given variables span;
#' location as for the Normal; and the Normal's conditional scale inflated
#' by `(df + d) / (df + k)`, `d` being the squared Mahalanobis distance of
#' `a` from the given variables' location.
#' @noRd
mv_t_conditional <- function(location, scale, df) {
  p <- length(location)
  vars <- names(location)
  size <- max(abs(diag(scale)))
  function(given, at) {
    rest <- setdiff(seq_len(p), given)
    s_rg <- scale[rest, given, drop = FALSE]
    s_gg <- scale[given, given, drop = FALSE]
    inv <- pinv_sym(s_gg, scale = size)
    dev <- at - location[given]
    off <- dev - as.numeric(s_gg %*% inv %*% dev)
    if (any(abs(off) > 1e-8 * max(1, sqrt(size), abs(dev)))) {
      return(dst_null())
    }
    k <- cov_root(s_gg, scale = size)$rank
    d <- as.numeric(t(dev) %*% inv %*% dev)
    weights <- s_rg %*% inv
    mu <- location[rest] + as.numeric(weights %*% dev)
    names(mu) <- vars[rest]
    s <- scale[rest, rest, drop = FALSE] - weights %*% t(s_rg)
    s <- clean_cov((s + t(s)) / 2 * (df + d) / (df + k), scale = size)
    dimnames(s) <- list(vars[rest], vars[rest])
    dst_mv_t(location = mu, scale = s, df = df + k)
  }
}

#' Probability that a multivariate t lies below (or above) each row of `x`.
#'
#' Writes the t as `location + Z / S`, with `S = sqrt(W / df)`, so that
#' `P(X <= x) = E[P(Z <= (x - location) * S)]`: an average of Normal
#' probabilities over `S`. The average is an integral over the quantiles of
#' `W`, which runs over `(0, 1)` whatever `df` is.
#' @param upper `TRUE` for the CDF, `FALSE` for the survival function.
#' @noRd
mvt_prob <- function(x, upper, location, scale, df) {
  p <- length(location)
  zero <- rep(0, p)
  vapply(seq_len(nrow(x)), function(i) {
    row <- x[i, ]
    if (anyNA(row)) {
      return(NA_real_)
    }
    dev <- row - location
    integrand <- function(u) {
      s <- sqrt(stats::qchisq(u, df = df) / df)
      z <- outer(s, dev)
      if (upper) {
        mvnorm_prob(lower = -Inf, upper = z, mean = zero, cov = scale)
      } else {
        mvnorm_prob(lower = z, upper = Inf, mean = zero, cov = scale)
      }
    }
    stats::integrate(
      integrand,
      lower = 0,
      upper = 1,
      rel.tol = 1e-8,
      subdivisions = 1000L
    )$value
  }, numeric(1))
}
