#' Conditional Distribution
#'
#' The distribution of some of a multivariate distribution's variables,
#' when the others are known.
#'
#' @param distribution A distribution of several variables.
#' @param given The known values, named after their variables: a named
#' numeric vector, such as `c(x = 3)`, or a named list.
#' @details
#' The result is a distribution of the variables not in `given`, in their
#' original order. It is what the `given` argument of [eval_mv_cdf()] and
#' the like evaluates, but as a distribution in its own right, to be
#' evaluated, drawn from, or conditioned further.
#'
#' ## Slices
#'
#' Conditioning on a variable that is a function of others slices the
#' distribution. To slice a distribution of \eqn{(X, Y)} along the line
#' \eqn{X + Y = s}, include \eqn{S = X + Y} as a third variable and
#' condition on it: the result is still a distribution of \eqn{X} and
#' \eqn{Y}, but it lives on a line, so its variable type is `"singular"`.
#' Take a [marginal()] of one of them for a distribution with a density.
#' For the multivariate Normal this is exact, since the three variables are
#' again Normal (with a singular covariance); see the "Multivariate
#' Distributions" vignette.
#'
#' ## How it is found
#'
#' A distribution can state its own conditionals (the multivariate Normal
#' does). Otherwise, for a distribution on finitely many points, the result
#' keeps the points that match `given`, with their probabilities rescaled.
#' For a continuous distribution, its density is the joint density divided
#' by the density of the `given` variables, and, when one variable is left,
#' its CDF comes from integrating that density. Its support is then taken
#' to be the support of the remaining variables in the joint distribution,
#' which may be larger than the conditional distribution needs.
#'
#' Conditioning on values that cannot occur --- a point not in a finite
#' distribution's support, or where the `given` variables have zero
#' density --- gives the Null distribution ([dst_null()]).
#' @returns A distribution: univariate if one variable is left over.
#' @seealso [marginal()]; the `given` argument of [eval_mv_cdf()].
#' @examples
#' d <- dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
#' conditional(d, given = c(x = 1))
#'
#' e <- dst_mv_empirical(list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2)))
#' conditional(e, given = list(a = 2))
#' @export
conditional <- function(distribution, given) {
  checkmate::assert_class(distribution, "dst")
  if (!is_multivariate(distribution)) {
    stop("A distribution of one variable has nothing to condition on.")
  }
  known <- is.numeric(given) || is.list(given) ||
    (is.logical(given) && all(is.na(given)))
  if (!known || length(given) == 0) {
    stop(
      "`given` must be a named vector of known values,\n",
      "as in `c(x = 3)`."
    )
  }
  nms <- rlang::names2(given)
  if (any(nms == "")) {
    stop(
      "Name each value in `given` after its variable,\n",
      "as in `c(x = 3)`."
    )
  }
  if (is.list(given) && any(lengths(given) != 1L)) {
    stop("Each value in `given` must be a single number.")
  }
  at <- as.numeric(unlist(given, use.names = FALSE))
  idx <- resolve_variables(distribution, nms, "given")
  if (length(idx) == dimension(distribution)) {
    stop(
      "Every variable is `given`, which leaves nothing to describe.\n",
      "Leave at least one variable out of `given`."
    )
  }
  if (anyNA(at)) {
    return(dst_null())
  }
  eval_property(distribution, "conditional", idx, at)
}

#' Conditional distribution, for the property network.
#' @param given Integer positions of the known variables.
#' @param at Their values, in the same order.
#' @noRd
eval_mv_conditional_from_network <- function(distribution, given, at) {
  p <- dimension(distribution)
  rest <- setdiff(seq_len(p), given)
  s <- support(distribution)
  name <- paste("Conditional", pretty_name(distribution))
  pts <- enumerate_points(s)
  if (!is.null(pts) && has_stated(distribution, "pmf")) {
    probs <- eval_joint(distribution, "pmf", as.list(pts))
    on <- rowSums(
      as.matrix(pts[given]) == rep(at, each = nrow(pts))
    ) == length(given)
    on <- on & probs > 0
    if (!any(on)) {
      return(dst_null())
    }
    kept <- pts[on, rest, drop = FALSE]
    probs <- probs[on] / sum(probs[on])
    if (length(rest) == 1L) {
      return(dst_finite(kept[[1L]], probs = probs))
    }
    rownames(kept) <- NULL
    return(mv_finite(kept, probs, name = name))
  }
  if (vtype(distribution) != "continuous") {
    stop(
      "Cannot find this conditional distribution. It can be worked out\n",
      "for a continuous distribution with a density, or a finite one;\n",
      "otherwise, the distribution must state a `conditional` property."
    )
  }
  margin <- eval_joint(
    marginal(distribution, given),
    "density",
    as.list(at)
  )
  if (is.na(margin) || margin <= 0) {
    return(dst_null())
  }
  # Put the known values back alongside the free ones, so that the joint
  # distribution can be evaluated.
  fill <- function(args) {
    args <- vctrs::vec_recycle_common(!!!args)
    n <- length(args[[1L]])
    full <- vector("list", p)
    full[rest] <- args
    full[given] <- lapply(at, rep, n)
    full
  }
  reps <- list(
    density = function(...) {
      eval_joint(distribution, "density", fill(list(...))) / margin
    }
  )
  if (length(rest) == 1L) {
    reps$cdf <- function(...) {
      eval_conditional(distribution, "cdf", fill(list(...)), given)
    }
    reps$survival <- function(...) {
      eval_conditional(distribution, "survival", fill(list(...)), given)
    }
  }
  build <- get("distribution", mode = "function")
  suppressWarnings(rlang::exec(
    build,
    !!!reps,
    .support = support_marginal(s, rest),
    .name = name
  ))
}
