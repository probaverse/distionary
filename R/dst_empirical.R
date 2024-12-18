#' Create an Empirical Distribution
#'
#' An empirical distribution is a non-parametric way to
#' estimate a distribution using data. By default,
#' it assigns equal probability to all observations
#' (this can be overridden with the `weights` argument).
#' Identical to [dst_finite()] with weights as probabilities,
#' except weights need not add to 1.
#'
#' @param y <`data-masking`>
#'   Outcomes to comprise the distribution. Should either
#'   evaluate to an (atomic) vector, or be a name in the specified data.
#' @param data Data frame containing the outcomes `y` and/or
#'   `weights`. Optional.
#' @param weights <`data-masking`>
#'   Weights to assign each outcome in `y`. Will be
#'   normalized so that the weights add up to 1
#'   (unlike [dst_finite()]),
#'   representing probabilities.
#' @param ... Additional arguments, currently not used.
#' @returns A distribution.
#' @seealso [dst_finite()]
#' @examples
#' x <- rnorm(100)
#' dst_empirical(x)
#' dst_empirical(value, data = data.frame(value = x))
#' @export
dst_empirical <- function(y, data, weights = 1, ...) {
  enquo_y <- rlang::enquo(y)
  enquo_w <- rlang::enquo(weights)
  if (missing(data)) {
    y <- rlang::eval_tidy(enquo_y)
    w <- rlang::eval_tidy(enquo_w)
  } else {
    y <- rlang::eval_tidy(enquo_y, data = data)
    w <- rlang::eval_tidy(enquo_w, data = data)
  }
  if (length(y) == 0L) {
    warning(
      "Can't make an empirical distribution from empty data. ",
      "Returning an empty distribution."
    )
    return(dst_null())
  }
  if (any(w < 0, na.rm = TRUE)) {
    stop("Weights must not be negative.")
  }
  if (length(w) == 1L) {
    w <- rep(w, length(y))
  }
  if (length(w) < length(y)) {
    stop("Not enough weights to match outcomes `y`.")
  }
  if (length(w) > length(y)) {
    stop("Not enough outcomes `y` to match weights.")
  }
  steps <- aggregate_weights(y, w, sum_to_one = TRUE)
  if (any(is.infinite(steps$location))) {
    stop("Possible outcomes of a distribution cannot be infinite.")
  }
  if (nrow(steps) == 1L) {
    return(dst_degenerate(steps$location))
  }
  res <- list(probabilities = steps)
  new_finite(res, variable = "discrete")
}

#' @rdname is_finite
#' @export
is.empirical <- function(object) inherits(object, "finite")

#' @rdname is_finite
#' @export
is_empirical <- function(object) inherits(object, "finite")
