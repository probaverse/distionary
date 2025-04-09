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
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user
#' wants this behaviour, it should be explicitly specified in their code,
#' because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#' @srrstats {G2.16} This version of distionary does force the propagation of
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user
#' specification for length-stability, also because `Inf` and `-Inf` are
#' expected in some cases (e.g., the support of any Normal distribution).
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
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
