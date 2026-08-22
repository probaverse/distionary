#' Probability Density Function
#'
#' Access a distribution's probability density function (pdf).
#'
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param definition Which definition of a density to hold the distribution
#' to. Under `"extended"`, the default, the density is the derivative of the
#' cdf wherever that derivative exists, which is an answer any distribution
#' can give. Under `"strict"`, it is a density in the full sense --- a
#' function that integrates to the distribution's probability --- which only
#' a continuous distribution has, and asking a distribution with atoms for
#' one is an error.
#'
#' This is a question about the distribution, not about which function to
#' evaluate: where both definitions apply they give the same numbers, and
#' `"strict"` only refuses where a density does not exist.
#' @examples
#' d <- dst_unif(0, 4)
#' eval_density(d, at = 0:4)
#' enframe_density(d, at = 0:4)
#'
#' # A discrete distribution places its probability on atoms, so it
#' # has no density in the strict sense. Under the extended reading,
#' # the cdf's derivative is 0 between the atoms and undefined on them.
#' eval_density(dst_pois(5), at = c(0.5, 1, 1.5))
#' try(eval_density(dst_pois(5), at = 0:4, definition = "strict"))
#' @family distributional representations
#' @rdname density
#' @export
eval_density <- function(
  distribution,
  at,
  ...,
  definition = c("extended", "strict")
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  definition <- match_variant(definition, "definition", "density")
  if (definition == "strict") {
    check_strict_definition(distribution, "density")
  }
  eval_property(distribution, "density", at)
}

#' @noRd
eval_density_from_network <- function(distribution, at) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  s <- support(distribution)
  if (is.null(s) || nrow(s[["continuous"]]) > 0) {
    # Wherever a distribution spreads probability over an interval, the height
    # it spreads it at is a fact about the distribution that nothing else can
    # supply --- so a missing density there stays missing.
    stop(
      "Cannot find density function. Density must be specified ",
      "in the distribution."
    )
  }
  # A distribution of nothing but atoms has no density, but its cdf still has
  # a derivative: zero between the atoms, undefined on them. That is the
  # extended reading, and the strict one has already refused by this point.
  out <- rep(0, length(at))
  is_known <- !is.na(at)
  out[!is_known] <- at[!is_known]
  known <- which(is_known)
  if (length(known) > 0) {
    out[known[support_has_atom(s, at[known])]] <- NaN
  }
  out
}

#' Refuse a representation the distribution does not have
#'
#' The strict reading of a density or a mass function asks for the real
#' thing, and the real thing exists only for one variable type each. This is
#' settled from the distribution's support before any representation is
#' consulted, because it is a fact about the distribution rather than a choice
#' between functions --- so it is not a variant [variants()] can declare.
#'
#' @param distribution Distribution object.
#' @param entry `"density"` or `"pmf"`.
#' @returns `NULL`, invisibly, if the representation exists; an error if not.
#' @noRd
check_strict_definition <- function(distribution, entry) {
  type <- vtype(distribution)
  if (is.null(type) || is.na(type)) {
    # The Null distribution declines to have a variable type, and answers
    # every question with `NA` rather than an error.
    return(invisible(NULL))
  }
  required <- if (entry == "density") "continuous" else "discrete"
  if (identical(type, required)) {
    return(invisible(NULL))
  }
  object <- if (entry == "density") {
    "density function"
  } else {
    "probability mass function"
  }
  reason <- if (entry == "density") {
    paste(
      "the probability it places on its atoms cannot be recovered by",
      "integrating anything"
    )
  } else {
    paste(
      "the probability it spreads over its continuous part sits on no",
      "point in particular"
    )
  }
  stop(
    "A ", type, " distribution has no ", object, " in the strict sense: ",
    reason, ".\n",
    "Use `definition = \"extended\"` to evaluate it anyway, where it is ",
    "read as the derivative of the cdf (for a density) or the size of the ",
    "cdf's jump (for a mass function)."
  )
}

#' @rdname density
#' @export
enframe_density <- function(
  ...,
  at,
  definition = "extended",
  arg_name = ".arg",
  fn_prefix = "density",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_density,
    fn_args = list(definition = definition)
  )
}
