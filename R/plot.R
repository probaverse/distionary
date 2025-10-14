#' Plot a Distribution
#'
#' Plot a distribution's representation.
#'
#' @param x Distribution object
#' @param what Name of the representation to plot.
#' @param ... Other arguments to pass to the `graphics::curve` function,
#' or `graphics::plot` in the case of the PMF.
#' @returns This function is run for its graphics byproduct, and therefore
#' returns the original distribution, invisibly.
#' @examples
#' d <- dst_norm(0, 1)
#' plot(d, from = -4, to = 4)
#' plot(d, "cdf", n = 1000)
#' plot(d, "survival")
#' plot(d, "quantile")
#' plot(d, "hazard")
#' plot(d, "chf")
#'
#' p <- dst_pois(4)
#' plot(p)
#' @srrstats {G2.3a} `match.arg()` is used in the `plot()` method for
#' distributions, when specifying the representation to plot. --> Copied
#' to `plot()` function.
#' @export
plot.dst <- function(
  x,
  what = c("density", "pmf", "cdf", "survival", "quantile", "hazard", "chf"),
  ...
) {
  ellipsis <- rlang::list2(...)
  specified <- length(what) == 1
  fname <- match.arg(what)
  if (specified) {
    if (fname == "density" && vtype(x) != "continuous") {
      stop("Density function does not exist.")
    }
    if (fname == "pmf" && vtype(x) != "discrete") {
      warning("Probability mass function does not exist.")
    }
  } else {
    if (fname == "density" && vtype(x) == "discrete") {
      fname <- "pmf"
    }
  }
  if (is.null(ellipsis[["ylab"]])) {
    ellipsis[["ylab"]] <- fname
  }
  if (fname == "quantile") {
    if (is.null(ellipsis[["from"]])) {
      ellipsis[["from"]] <- 0
    }
    if (is.null(ellipsis[["to"]])) {
      ellipsis[["to"]] <- 1
    }
    if (is.null(ellipsis[["xlab"]])) {
      ellipsis[["xlab"]] <- "Probability"
    }
    f <- representation_as_function(x, "quantile")
    ellipsis[["expr"]] <- as.name("f")
    do.call(graphics::curve, args = ellipsis)
    return(invisible(x))
  }
  if (is.null(ellipsis[["from"]])) {
    q0 <- range(x)[1L]
    if (q0 == -Inf) {
      ellipsis[["from"]] <- eval_quantile(x, at = 0.001)
    } else {
      ellipsis[["from"]] <- q0
    }
  }
  if (is.null(ellipsis[["to"]])) {
    q1 <- range(x)[2L]
    if (q1 == Inf) {
      ellipsis[["to"]] <- eval_quantile(x, at = 0.999)
    } else {
      ellipsis[["to"]] <- q1
    }
  }
  if (is.null(ellipsis[["xlab"]])) {
    ellipsis[["xlab"]] <- "y"
  }
  if (fname == "pmf") {
    if (!pretty_name(x) %in% c(
      "Bernoulli", "Binomial", "Poisson", "Geometric", "Negative Binomial",
      "Hypergeometric", "Degenerate", "Finite"
    )) {
      warning(
        "This version of distionary assumes the distribution takes on ",
        "integer values. If this is not the case, then the displayed pmf ",
        "may not be representative."
      )
    }
    xvals <- ellipsis[["from"]]:ellipsis[["to"]]
    if (pretty_name(x) == "Finite") {
      outcomes <- parameters(x)[["outcomes"]]
      xvals <- outcomes[
        outcomes >= ellipsis[["from"]] &
          outcomes <= ellipsis[["to"]]
      ]
    }
    ellipsis[["from"]] <- NULL
    ellipsis[["to"]] <- NULL
    ellipsis[["x"]] <- xvals
    ellipsis[["y"]] <- eval_pmf(x, at = xvals)
    do.call("plot", args = ellipsis)
    return(invisible(x))
  }
  f <- representation_as_function(x, fname)
  ellipsis[["expr"]] <- as.name("f")
  do.call("curve", args = ellipsis)
  invisible(x)
}
