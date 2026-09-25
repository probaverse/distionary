#' Conditional distribution, for the property network.
#'
#' A multivariate distribution may state a `conditional` property: a function
#' of the positions of the known variables and their values, returning the
#' distribution of the rest. When it does not, this works one out. It is what
#' the `given` argument of `eval_mv_cdf()` and friends relies on, and what
#' distplyr's `conditional()` verb calls, through
#' `eval_property(d, "conditional", given, at)`.
#'
#' For a distribution on finitely many points, the result keeps the points
#' matching `at`, rescaled. For a continuous one, its density is the joint
#' density over that of the known variables, and with one variable left its
#' CDF comes from integrating that; its support is the remaining variables'
#' support in the joint distribution, which may be larger than needed. Values
#' that cannot occur give the Null distribution.
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
    marginal_at(distribution, given),
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
