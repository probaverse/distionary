#' Declare Variants of a Representation
#'
#' Bundle a representation function together with alternative *variants* of
#' it, for use in [distribution()]. A plain function supplied to
#' `distribution()` promises one thing: the representation in its canonical
#' variant. `variants()` is how a distribution promises more --- a right
#' inverse of the CDF, say, alongside the usual left one.
#'
#' @param .f The canonical variant of the representation: an ordinary
#' function, evaluated the way the matching `eval_` function documents.
#' `NULL` if the distribution can only provide the variants named in `...`.
#' @param ... Named functions, one per variant, named by the *level* of the
#' variant they provide (`right = `, `strict = `). Which levels are available
#' depends on the representation --- they are the values accepted by the
#' variant arguments of the matching `eval_` function, such as the `side`
#' argument of [eval_quantile()].
#' @param .name The name of the representation being built, such as
#' `"quantile"`. Rarely needed: [distribution()] fills this in from the
#' argument the representation is assigned to, which is also when the declared
#' levels are checked. Supply it to build a representation on its own, away
#' from a `distribution()` call.
#' @returns A representation object. It is also a function --- the canonical
#' variant --- so anything that can use a plain representation can use this.
#' @details
#' A variant that is *not* declared here is not unavailable, only underived:
#' distionary works it out from the distribution's other representations, the
#' same way it works out a quantile function from a CDF. Declaring one says
#' "here is a better route than the one you would find yourself", which is
#' worth doing when the derivation is slow, imprecise, or impossible.
#'
#' Not every argument of an `eval_` function is a variant that can be declared
#' here. Some, such as the `definition` argument of [eval_density()], ask
#' whether a representation exists for this distribution at all rather than
#' asking for a different function, and are settled before any representation
#' is consulted. Declaring one is an error, since nothing would ever call it.
#' @examples
#' # The Poisson quantile function is the left inverse of its CDF. The
#' # right inverse steps to the next atom where a probability lands on
#' # top of a jump, and agrees with the left one everywhere else.
#' d <- distribution(
#'   cdf = function(x) ppois(x, lambda = 5),
#'   pmf = function(x) dpois(x, lambda = 5),
#'   quantile = variants(
#'     function(p) qpois(p, lambda = 5),
#'     right = function(p) {
#'       q <- qpois(p, lambda = 5)
#'       q + (ppois(q, lambda = 5) <= p)
#'     }
#'   ),
#'   .support = discrete(natural0()),
#'   .name = "Poisson"
#' )
#' eval_quantile(d, at = c(0.2, 0.5), side = "left")
#' eval_quantile(d, at = c(0.2, 0.5), side = "right")
#' @family Distribution Construction
#' @export
variants <- function(.f = NULL, ..., .name = NULL) {
  declared <- rlang::list2(...)
  if (!is.null(.f) && !is.function(.f)) {
    stop(
      "`.f` must be the canonical variant of the representation, as a ",
      "function, or `NULL` if the distribution cannot provide it."
    )
  }
  checkmate::assert_character(.name, len = 1, null.ok = TRUE)
  if (length(declared) > 0) {
    checkmate::assert_list(declared, types = "function", names = "named")
  }
  if (is.null(.f) && length(declared) == 0) {
    stop(
      "A representation needs at least one function: a canonical variant in ",
      "`.f`, or a named variant in `...`."
    )
  }
  out <- new_representation(canonical = .f, declared = declared)
  if (!is.null(.name)) {
    out <- bind_representation(out, .name)
  }
  out
}

#' Constructor for representation objects
#'
#' The object is the canonical function itself, carrying its variants as
#' attributes, so that every code path that treats a representation as a plain
#' function keeps working.
#'
#' @param canonical The canonical variant, or `NULL`.
#' @param declared Named list of functions, named by variant level, as the
#' user wrote them. Kept whether or not the object has been bound, so that
#' printing an unbound representation can still say what it holds.
#' @param bound The same functions, resolved against a representation name: a
#' list of `list(spec = , fn = )` entries, where `spec` is a named list such as
#' `list(side = "right")`. `NULL` until the object is bound.
#' @param name The representation's name, or `NULL` if not yet bound.
#' @returns A representation object.
#' @noRd
new_representation <- function(
  canonical,
  declared,
  bound = NULL,
  name = NULL
) {
  f <- canonical
  if (is.null(f)) {
    f <- function(...) {
      stop(
        "This representation declares only variants, and was asked for the ",
        "canonical one. Give `variants()` a function in `.f` to provide it."
      )
    }
  }
  structure(
    f,
    class = c("representation", "function"),
    canonical = canonical,
    declared = declared,
    bound = bound,
    representation_name = name
  )
}

#' Is this object a representation with declared variants?
#' @param x Object to test.
#' @returns `TRUE` or `FALSE`.
#' @noRd
is_representation <- function(x) {
  inherits(x, "representation")
}

#' Bind a representation to the name it is stored under
#'
#' Until a representation knows what it is a representation *of*, its declared
#' levels are just names: there is nothing to check them against. Binding
#' resolves each level to the variant argument that accepts it, and errors on
#' a level no argument accepts. [distribution()] binds every representation it
#' is given, so the check lands at construction time.
#'
#' @param x A representation object.
#' @param name Name it is stored under, such as `"quantile"`.
#' @returns The representation, bound.
#' @noRd
bind_representation <- function(x, name) {
  checkmate::assert_class(x, "representation")
  checkmate::assert_character(name, len = 1)
  existing <- attr(x, "representation_name")
  if (!is.null(existing)) {
    if (!identical(existing, name)) {
      stop(
        "This representation was built for '", existing, "', but is being ",
        "stored as '", name, "'."
      )
    }
    return(x)
  }
  declared <- attr(x, "declared")
  allowed <- declarable_variants(name)
  bound <- list()
  for (level in names(declared)) {
    argument <- variant_argument(level, allowed, name)
    spec <- list(level)
    names(spec) <- argument
    bound[[length(bound) + 1L]] <- list(
      spec = spec,
      fn = declared[[level]]
    )
  }
  new_representation(attr(x, "canonical"), declared, bound, name)
}

#' Bind every representation in a list to its own name
#'
#' @param representations Named list, as collected by [distribution()].
#' @returns The same list, with representation objects bound.
#' @noRd
bind_representations <- function(representations) {
  nms <- names(representations)
  for (i in seq_along(representations)) {
    if (is_representation(representations[[i]])) {
      representations[[i]] <- bind_representation(
        representations[[i]],
        nms[[i]]
      )
    }
  }
  representations
}

#' Find the variant argument that accepts a level
#'
#' @param level A single level, such as `"right"`.
#' @param allowed Named list of allowed levels, from `declarable_variants()`.
#' @param name The representation's name, for the error message.
#' @returns The name of the variant argument accepting `level`.
#' @noRd
variant_argument <- function(level, allowed, name) {
  for (argument in names(allowed)) {
    if (level %in% allowed[[argument]]) {
      return(argument)
    }
  }
  if (length(allowed) == 0) {
    stop(
      "'", name, "' has no variants to declare, so there is nothing for the ",
      "'", level, "' function to provide."
    )
  }
  stop(
    "'", level, "' is not a variant of '", name, "'. Available: ",
    paste0("'", unlist(allowed, use.names = FALSE), "'", collapse = ", "),
    "."
  )
}

#' @export
print.representation <- function(x, ...) {
  name <- attr(x, "representation_name")
  if (is.null(name)) {
    cat("<representation>\n")
  } else {
    cat(sprintf("<representation: %s>\n", name))
  }
  if (is.null(attr(x, "canonical"))) {
    cat("-- canonical --\n(none)\n")
  } else {
    cat("-- canonical --\n")
    print(attr(x, "canonical"))
  }
  declared <- attr(x, "declared")
  if (length(declared) > 0) {
    bound <- attr(x, "bound")
    for (i in seq_along(declared)) {
      level <- names(declared)[[i]]
      if (is.null(bound)) {
        cat(sprintf("-- %s --\n", level))
      } else {
        cat(sprintf(
          "-- %s = \"%s\" --\n",
          names(bound[[i]][["spec"]]),
          level
        ))
      }
      print(declared[[i]])
    }
  }
  invisible(x)
}
