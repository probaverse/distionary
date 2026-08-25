#' Variants of a Distributional Representation
#'
#' A *variant* is a version of a representation that answers a different
#' question about the same distribution: the right inverse of a CDF rather
#' than the left one, the probability strictly below a value rather than at or
#' below it. Variants are ordinary arguments of the `eval_` functions, and
#' those arguments are the whole specification --- there is no separate list
#' of them to consult, and no second place to keep up to date.
#'
#' An argument of `eval_<entry>()` other than `distribution` and `at` is a
#' variant argument. Its levels, and which of them is canonical, are read off
#' its default in the usual R idiom:
#'
#' ``` r
#' eval_quantile <- function(distribution, at, ..., side = c("left", "right"))
#' ```
#'
#' `side` takes "left" or "right", and "left" is canonical --- the variant a
#' plain function supplied to [distribution()] is taken to provide.
#'
#' @name variants-internal
#' @keywords internal
NULL

#' Cache of what has been read off the `eval_` functions
#'
#' Reading a representation's variants means pulling apart a function's
#' formals, which is a lot of work to repeat: `eval_cdf()` is called hundreds
#' of times over in a single quantile inversion. The answer cannot change
#' within a session, so it is worked out once per representation and kept.
#' @noRd
the <- new.env(parent = emptyenv())
the[["levels"]] <- list()
the[["canonical"]] <- list()
the[["declarable"]] <- list()

#' Look up a function by name within the package
#'
#' @param name Name of the function to find.
#' @returns The function, or `NULL` if there is no such function.
#' @noRd
find_function <- function(name) {
  if (!exists(name)) {
    return(NULL)
  }
  f <- get(name)
  if (!is.function(f)) {
    return(NULL)
  }
  f
}

#' The variant arguments of a representation's `eval_` function
#'
#' @param entry Name of the representation, such as `"quantile"`.
#' @returns A named list of the variant arguments, each holding that
#' argument's default. Empty if the representation has no variants, or if
#' distionary has no `eval_` function for it.
#' @noRd
variant_arguments <- function(entry) {
  f <- find_function(paste0("eval_", entry))
  if (is.null(f)) {
    return(list())
  }
  arguments <- formals(f)
  arguments[!names(arguments) %in% c("distribution", "at", "...")]
}

#' The levels each of a representation's variant arguments accepts
#'
#' @param entry Name of the representation, such as `"quantile"`.
#' @returns A named list, one entry per variant argument, holding that
#' argument's default evaluated: the allowed strings, for one matched against
#' a set of them.
#' @noRd
variant_levels <- function(entry) {
  cached <- the[["levels"]][[entry]]
  if (!is.null(cached)) {
    return(cached)
  }
  out <- lapply(variant_arguments(entry), function(default) {
    tryCatch(eval(default), error = function(e) NULL)
  })
  the[["levels"]][[entry]] <- out
  out
}

#' Match a variant argument against the levels its `eval_` function allows
#'
#' The levels are read from that function's own signature rather than named
#' again here, so there is only ever one statement of what they are. This
#' stands in for a bare `rlang::arg_match()` call, which is too slow to put on
#' a path travelled hundreds of times per quantile inversion.
#'
#' @param value The value the argument was given.
#' @param argument The argument's name, such as `"side"`.
#' @param entry Name of the representation, such as `"quantile"`.
#' @returns The matched level, length 1.
#' @noRd
match_variant <- function(value, argument, entry) {
  levels <- variant_levels(entry)[[argument]]
  if (identical(value, levels)) {
    # Untouched by the caller: the first level is the canonical one.
    return(levels[[1L]])
  }
  rlang::arg_match0(value, levels, arg_nm = argument)
}

#' The canonical value of each of a representation's variant arguments
#'
#' The canonical value is the argument's default: the first level of a set of
#' allowed strings, or the default itself for anything else.
#'
#' @param entry Name of the representation, such as `"quantile"`.
#' @returns A named list, one entry per variant argument.
#' @noRd
canonical_variant <- function(entry) {
  cached <- the[["canonical"]][[entry]]
  if (!is.null(cached)) {
    return(cached)
  }
  out <- lapply(variant_levels(entry), function(value) {
    if (is.character(value) && length(value) > 1L) {
      return(value[[1L]])
    }
    value
  })
  the[["canonical"]][[entry]] <- out
  out
}

#' The variants a representation may declare
#'
#' A variant can be declared with [variants()] when it is a choice between
#' functions that distionary routes to the representation --- which is to say,
#' when the matching `eval_*_from_network()` function takes it too. An
#' argument that `eval_<entry>()` settles by itself, such as the `definition`
#' argument of [eval_density()], asks whether the representation exists at all
#' rather than which function to call, so there is nothing to declare and
#' nothing that would ever call it.
#'
#' @param entry Name of the representation, such as `"quantile"`.
#' @returns A named list of the declarable variant arguments, each holding
#' that argument's allowed levels.
#' @noRd
declarable_variants <- function(entry) {
  cached <- the[["declarable"]][[entry]]
  if (!is.null(cached)) {
    return(cached)
  }
  all_levels <- variant_levels(entry)
  network <- find_function(paste0("eval_", entry, "_from_network"))
  routed <- if (is.null(network)) character() else names(formals(network))
  out <- list()
  for (name in names(all_levels)) {
    if (!name %in% routed) {
      next
    }
    levels <- all_levels[[name]]
    if (is.character(levels) && length(levels) > 1L) {
      out[[name]] <- levels
    }
  }
  the[["declarable"]][[entry]] <- out
  out
}

#' Describe a request as its departures from the canonical representation
#'
#' A variant carries only what differs from the plain representation, so that
#' a canonical request reaches a plainly-specified representation as the bare
#' call it has always been.
#'
#' @param ... Named variant values, as the `eval_` function received them.
#' @param .entry Name of the representation, such as `"quantile"`.
#' @returns A named list of the values that are not canonical, possibly empty.
#' @noRd
variant <- function(..., .entry) {
  # On the hot path: every `eval_` call passes through here, and a quantile
  # inversion makes hundreds of them. Kept to plain base calls for that
  # reason.
  requested <- list(...)
  if (length(requested) == 0) {
    return(list())
  }
  canonical <- canonical_variant(.entry)
  departs <- vapply(
    names(requested),
    function(name) !identical(requested[[name]], canonical[[name]]),
    FUN.VALUE = logical(1L)
  )
  if (!any(departs)) {
    return(list())
  }
  requested[departs]
}

#' Validate a variant supplied by a caller
#'
#' @param variant A named list, or `NULL`.
#' @returns The variant, as a named list.
#' @noRd
check_variant <- function(variant) {
  if (is.null(variant)) {
    return(list())
  }
  checkmate::assert_list(variant)
  if (length(variant) > 0) {
    checkmate::assert_names(names(variant), type = "unique")
  }
  variant
}

#' The function a representation offers for a variant, if it offers one
#'
#' @param representation The stored representation: a plain function, or a
#' representation object built by [variants()].
#' @param variant Named list of departures from the canonical variant.
#' @returns A function, or `NULL` if this representation does not provide the
#' variant asked for and it will have to be derived.
#' @noRd
representation_function <- function(representation, variant) {
  if (!is_representation(representation)) {
    # A plain function promises the canonical variant, and nothing else.
    if (length(variant) == 0) {
      return(representation)
    }
    return(NULL)
  }
  if (length(variant) == 0) {
    return(attr(representation, "canonical"))
  }
  bound <- attr(representation, "bound")
  if (is.null(bound)) {
    stop(
      "This representation has not been bound to a name, so its\n",
      "variants cannot be matched. Build it inside `distribution()`,\n",
      "or name it with the `.name` argument of `variants()`."
    )
  }
  for (entry in bound) {
    if (same_variant(entry[["spec"]], variant)) {
      return(entry[["fn"]])
    }
  }
  NULL
}

#' Do two variants ask for the same thing?
#'
#' @param x,y Named lists.
#' @returns `TRUE` or `FALSE`.
#' @noRd
same_variant <- function(x, y) {
  if (length(x) != length(y)) {
    return(FALSE)
  }
  identical(x[order(names(x))], y[order(names(y))])
}
