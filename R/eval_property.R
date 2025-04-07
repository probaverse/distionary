#' Evaluate a distribution
#'
#' Evaluate a distribution property. The distribution itself
#' is first searched for the property, and if it can't
#' be found, will attempt to calculate the property
#' from other entries.
#'
#' @param distribution Distribution object.
#' @param entry Name of the property, such as "cdf" or
#' "mean". Length 1 character vector.
#' @param ... If the property is a function, arguments to the
#' function go here. Need not be named; inserted in the order they
#' appear.
#' @returns The distribution's property, evaluated. If cannot be
#' evaluated, returns `NULL`.
#' @examples
#' d <- distribution(
#'   cdf = \(x) (x > 0) * pmin(x^2, 1),
#'   g = 9.81,
#'   .vtype = "continuous"
#' )
#' eval_property(d, "g")
#' eval_property(d, "quantile", 1:9 / 10)
#' eval_property(d, "mean")
#' eval_property(d, "realise", 10)
#' eval_property(d, "foofy")
#' eval_property(d, "foofy", 1:10)
#' @export
eval_property <- function(distribution, entry, ...) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_character(entry, len = 1)
  repres <- distribution[[entry]]
  if (is.null(repres)) {
    if (entry == "realize") {
      return(eval_property(distribution, "realise", ...))
    }
    eval_from_network <- paste0("eval_", entry, "_from_network")
    available <- exists(eval_from_network)
    if (!available) {
      return(NULL)
    } else {
      return(rlang::exec(eval_from_network, distribution, ...))
    }
  } else {
    if (is.function(repres)) {
      return(repres(...))
    } else {
      ellipsis::check_dots_empty()
      return(repres)
    }
  }
}
