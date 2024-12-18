#' Create a Finite Distribution
#'
#' A finite distribution assigns probabilities to a finite
#' collection of values. This includes categorical distributions.
#'
#' @param y <`data-masking`>
#'   Outcomes to comprise the distribution. Should either
#'   evaluate to an (atomic) vector, or be a name in the specified data.
#' @param probs <`data-masking`>
#'   Probabilities corresponding to the outcomes in `y`.
#'   Must not be negative, but **must sum to 1**
#'   (unlike [dst_empirical()]). Should either
#'   evaluate to a vector, or be a name in the specified data.
#' @param data Data frame containing the outcomes `y` and/or
#'   probabilities `probs`. Optional.
#' @param ... Additional arguments, currently not used.
#' @returns A distribution.
#' @note This distribution is called "finite" and not
#' "discrete", because a discrete distribution could have
#' an infinite amount of possible outcomes, as in the
#' Poisson distribution.
#' @seealso [dst_empirical()]
#' @examples
#' dst_finite(1:5, probs = rep(0.2, 5))
#' @export
dst_finite <- function(y, probs, data, ...) {
  enquo_y <- rlang::enquo(y)
  enquo_w <- rlang::enquo(probs)
  if (missing(data)) {
    y <- rlang::eval_tidy(enquo_y)
    w <- rlang::eval_tidy(enquo_w)
  } else {
    y <- rlang::eval_tidy(enquo_y, data = data)
    w <- rlang::eval_tidy(enquo_w, data = data)
  }
  if (length(y) == 0) {
    warning(
      "Can't make a finite distribution from empty data. ",
      "Returning an empty distribution."
    )
    return(dst_null())
  }
  if (length(w) < length(y)) {
    stop("Not enough probabilities to match outcomes `y`.")
  }
  if (length(w) > length(y)) {
    stop("Not enough outcomes `y` to match probabilities.")
  }
  if (any(w < 0, na.rm = TRUE)) {
    stop("Probabilities must not be negative.")
  }
  if (sum(probs) != 1) {
    stop(
      "Probabilities must add up to 1. ",
      "Perhaps you'd prefer to use `dst_empirical()`?"
    )
  }
  steps <- aggregate_weights(y, w, sum_to_one = FALSE)
  if (nrow(steps) == 1L) {
    return(dst_degenerate(steps$location))
  }
  res <- list(probabilities = steps)
  new_finite(res, variable = "discrete")
}


#' Constructor Function for Finite Distributions
#'
#' @param l List containing the components of a step distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#' or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_finite <- function(l, variable, ..., class = character()) {
  new_distribution(
    l,
    variable = variable,
    ...,
    class    = c(class, "finite")
  )
}

#' Is a distribution a finite distribution?
#'
#' There's no difference between checking whether a distribution
#' is finite or empirical -- the functions exist simply for
#' completeness.
#'
#' @param object Object to check
#' @rdname is_finite
#' @export
is_finite_dst <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is.finite_dst <- function(object) inherits(object, "finite")
