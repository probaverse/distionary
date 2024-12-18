#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param location Parameter of the distribution family.
#' @returns A distribution.
#' @examples
#' require(graphics)
#' d <- dst_degenerate(5)
#' plot(d, "quantile")
#' @rdname degenerate
#' @export
dst_degenerate <- function(location) {
  cant_be_numeric <- suppressWarnings(is.na(as.numeric(location)))
  if (cant_be_numeric) {
    stop("'location' parameter must be numeric.")
  }
  if (length(location) != 1L) {
    stop(
      "'location' parameter must contain exactly one number. ",
      "Received ", length(location)
    )
  }
  if (is.infinite(location)) {
    stop("Possible outcomes of a distribution cannot be infinite.")
  }
  as_table <- aggregate_weights(location, 1, sum_to_one = FALSE)
  res <- list(probabilities = as_table)
  new_finite(res, variable = "discrete", class = "degenerate")
}

#' @param object Object to test
#' @rdname degenerate
#' @export
is_degenerate <- function(object) {
  inherits(object, "degenerate")
}

#' @rdname degenerate
#' @export
is.degenerate <- function(object) {
  inherits(object, "degenerate")
}
