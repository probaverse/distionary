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
#' @param variant Which variant of the representation to evaluate, as a named
#' list holding only what departs from the canonical variant --- so
#' `list(side = "right")` for the right inverse of the CDF, and the default
#' `list()` for the representation as ordinarily understood. The `eval_`
#' functions assemble this from their own arguments, and calling one of them
#' is the usual way to reach a variant; see [eval_quantile()] for an example
#' and [variants()] for how a distribution provides its own.
#' @returns The distribution's property, evaluated. If cannot be
#' evaluated, returns `NULL`.
#' @details
#' A representation stored as a plain function is taken to provide the
#' canonical variant, and only that. Asking for any other variant leaves it
#' untouched and derives the answer from the distribution's other
#' representations --- correct either way, if sometimes slower than a
#' representation that provides the variant itself.
#' @examples
#' d <- distribution(
#'   cdf = function(x) {
#'     (x > 0) * pmin(x^2, 1)
#'   },
#'   g = 9.81,
#'   .support = continuous(c(0, 1))
#' )
#' eval_property(d, "g")
#' eval_property(d, "quantile", 1:9 / 10)
#' eval_property(d, "quantile", 1:9 / 10, variant = list(side = "right"))
#' eval_property(d, "mean")
#' eval_property(d, "realise", 10)
#' eval_property(d, "foofy")
#' eval_property(d, "foofy", 1:10)
#' @export
eval_property <- function(distribution, entry, ..., variant = list()) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_character(entry, len = 1)
  variant <- check_variant(variant)
  repres <- distribution[[entry]]
  if (is.null(repres)) {
    if (entry == "realize") {
      return(eval_property(distribution, "realise", ..., variant = variant))
    }
  } else if (is.function(repres)) {
    f <- representation_function(repres, variant)
    if (!is.null(f)) {
      return(f(...))
    }
    # The distribution has this representation, but not in the variant asked
    # for, so the variant is derived below just as a missing one would be.
  } else {
    if (length(variant) > 0) {
      stop(
        "The '", entry, "' entry of this distribution is a value, not a ",
        "function, so it has no variants."
      )
    }
    rlang::check_dots_empty()
    return(repres)
  }
  eval_from_network <- paste0("eval_", entry, "_from_network")
  available <- exists(eval_from_network)
  if (!available) {
    if (length(variant) > 0) {
      stop(
        "Cannot evaluate the ", describe_variant(variant), " variant of '",
        entry, "': this distribution does not provide it, and distionary ",
        "does not know how to derive it."
      )
    }
    return(NULL)
  }
  rlang::exec(eval_from_network, distribution, ..., !!!variant)
}

#' Describe a variant for an error message
#'
#' @param variant Named list of departures from the canonical variant.
#' @returns A single string, such as `side = "right"`.
#' @noRd
describe_variant <- function(variant) {
  described <- vapply(
    names(variant),
    function(name) {
      value <- variant[[name]]
      if (is.character(value)) {
        value <- paste0("\"", value, "\"")
      }
      paste0(name, " = ", format(value))
    },
    FUN.VALUE = character(1L)
  )
  paste(described, collapse = ", ")
}
