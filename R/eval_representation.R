#' Evaluate a distributional representation
#'
#' Evaluate a distributional representation. The distribution itself
#' is first searched for the representation / property, and if can't
#' be found, will attempt to calculate the representation / property
#' from other representations.
#'
#' @param distribution Distribution object.
#' @param entry Name of the representation / property, such as "cdf" or
#' "mean".
#' @param ... If the representation is a function, arguments to the
#' function go here. Need not be named.
#' @returns The distribution's representation, evaluated. If cannot be
#' evaluated, returns `NULL`.
#' @examples
#' d <- distribution(cdf = \(x) (x > 0) * pmin(x^2, 1), g = 9.81)
#' eval_representation(d, "g")
#' eval_representation(d, "quantile", 1:9 / 10)
#' eval_representation(d, "mean")
#' eval_representation(d, "realise", 10)
#' eval_representation(d, "foofy")
#' eval_representation(d, "foofy", 1:10)
#' @export
eval_representation <- function(distribution, entry, ...) {
  repres <- distribution[[entry]]
  if (is.null(repres)) {
    if (entry == "realize") {
      return(eval_representation(distribution, "realise", ...))
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

