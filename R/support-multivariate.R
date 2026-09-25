#' The Support of a Multivariate Distribution
#'
#' A multivariate distribution places probability on points with several
#' coordinates, one per variable. `support_product()` builds its support the
#' way [expand.grid()] builds a data frame: every value one variable can take
#' is paired with every value each of the others can take.
#'
#' @param ... Supports, one per variable (or several variables, if the
#' support is itself multivariate). Name them to name the variables:
#' `support_product(flow = continuous(c(0, Inf)), gauge = discrete(1:3))`.
#' Unnamed univariate supports get the names `x1`, `x2`, and so on, by
#' position.
#' @details
#' The variables of a product support are the variables of its pieces, in
#' order, so a product of a bivariate support and a univariate one has three
#' variables. A multivariate piece brings its own variable names and cannot be
#' renamed by naming its argument.
#'
#' Not every multivariate support is a product. Two things are allowed here
#' that are not:
#'
#' - A finite set of points, built by handing [discrete()] a data frame (or a
#'   matrix) with one column per variable and one row per point. This is the
#'   support of an empirical distribution: the points observed, and not every
#'   pairing of an observed `x` with an observed `y`.
#' - A product with such a set as one of its pieces.
#'
#' Supports that are neither --- a triangle such as `y <= x`, or a line such
#' as `x + y = 100` --- cannot be described yet.
#'
#' The variable type ([vtype()]) of a multivariate support describes the
#' joint distribution: `"continuous"` when every piece is continuous,
#' `"discrete"` when every piece is discrete, and `"mixed"` otherwise.
#' @returns A support object (inheriting class `"support"`).
#' @seealso [dimension()] and [variables()] to count and name the variables.
#' @examples
#' # The support of a bivariate Normal.
#' support_product(x = continuous(), y = continuous())
#'
#' # A count paired with an amount.
#' support_product(
#'   storms = discrete(natural0()),
#'   depth = continuous(c(0, Inf))
#' )
#'
#' # Finitely many points, not a product.
#' discrete(data.frame(x = c(1, 2, 2), y = c(5, 5, 6)))
#' @family Support
#' @export
support_product <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    stop("A product needs at least one support to multiply.")
  }
  arg_names <- rlang::names2(dots)
  factors <- list()
  vars <- character(0)
  for (i in seq_along(dots)) {
    s <- as_support(dots[[i]])
    if (inherits(s, "support_product")) {
      if (arg_names[[i]] != "") {
        stop(
          "A multivariate support brings its own variable names,\n",
          "so argument `", arg_names[[i]], "` cannot rename it.\n",
          "Name the variables where that support is built."
        )
      }
      factors <- c(factors, s[["factors"]])
      vars <- c(vars, s[["variables"]])
    } else if (inherits(s, "support_mv")) {
      if (arg_names[[i]] != "") {
        stop(
          "A multivariate support brings its own variable names,\n",
          "so argument `", arg_names[[i]], "` cannot rename it.\n",
          "Name the variables where that support is built."
        )
      }
      factors <- c(factors, list(s))
      vars <- c(vars, support_variables(s))
    } else {
      factors <- c(factors, list(s))
      vars <- c(vars, arg_names[[i]])
    }
  }
  vars <- fill_variable_names(vars)
  # A product of one support is that support, not a wrapper around it.
  if (length(factors) == 1L) {
    return(factors[[1L]])
  }
  new_support_product(factors, vars)
}

#' Number of Variables
#'
#' `dimension()` gives how many variables a distribution (or a support)
#' describes, and `variables()` gives their names. `variables<-` renames
#' them.
#'
#' @param x A distribution or a support. For `variables<-`, a distribution.
#' @param value New variable names: one per variable, all different.
#' @details
#' The dimension is the number of variables, which is the number of
#' coordinates a point in the support has. It is not the dimension of the
#' support itself as a geometric object: a bivariate Normal whose two
#' variables are perfectly correlated places all of its probability on a
#' line, but it is still a distribution of two variables, and its dimension
#' is 2. This is the sense in which probability speaks of a "p-dimensional
#' random vector", whether or not its distribution is degenerate.
#'
#' The name is not `dim()` because that already means something in R: the
#' extents of an array, whose product is the length of the object. A
#' distribution has length 1 (see [length.dst()]), so a `dim()` of 2 would
#' contradict it, and functions such as [NROW()] would take a bivariate
#' distribution to be two rows of something.
#'
#' ## Names
#'
#' Every distribution names its variables, as a data frame names its
#' columns, and a name is given when none is: `x` for a univariate
#' distribution, `x` and `y` from the `dst_bi_*()` constructors, and `x1`,
#' `x2`, and so on otherwise. The names are how [prob()] refers to the
#' variables, and they carry through [marginal()] and conditioning.
#'
#' A univariate *support* has no name, since a support describes values, not
#' a variable; `variables()` gives `NULL` for it. The Null distribution
#' ([dst_null()]) has no support, so its dimension is not known (`NA`), and
#' nor are its names (`NULL`).
#' @returns For `dimension()`, a single integer. For `variables()`, a
#' character vector with one name per variable. `variables<-` returns the
#' renamed distribution.
#' @examples
#' dimension(dst_norm(0, 1))
#' d <- dst_bi_norm(mean = c(0, 0), sd = c(1, 1), cor = 0.5)
#' dimension(d)
#' variables(d)
#' variables(dst_norm(0, 1))
#' dimension(support_product(a = continuous(), b = discrete(0:3)))
#'
#' variables(d) <- c("flow", "depth")
#' d
#' @export
dimension <- function(x) {
  s <- as_support_arg(x, absent = "null")
  if (is.null(s)) {
    return(NA_integer_)
  }
  support_dimension(s)
}

#' @rdname dimension
#' @export
variables <- function(x) {
  s <- as_support_arg(x, absent = "null")
  if (is.null(s)) {
    return(NULL)
  }
  if (inherits(x, "dst") && !inherits(s, "support_mv")) {
    # A univariate distribution keeps its name beside the support, which
    # describes values rather than a variable.
    name <- attr(x, "variable")
    return(if (is.null(name)) "x" else name)
  }
  support_variables(s)
}

#' @rdname dimension
#' @export
`variables<-` <- function(x, value) {
  checkmate::assert_class(x, "dst")
  s <- support(x)
  if (is.null(s)) {
    stop("The Null distribution has no variables to name.")
  }
  p <- support_dimension(s)
  if (!is.character(value) || length(value) != p || anyNA(value) ||
    any(value == "")) {
    stop(
      "Give one name per variable (", p, "), none of them empty."
    )
  }
  if (anyDuplicated(value)) {
    stop(
      "Each variable needs its own name, but `",
      value[duplicated(value)][[1L]], "` is used twice."
    )
  }
  if (p == 1L) {
    attr(x, "variable") <- value
    return(x)
  }
  attr(x, "support") <- rename_support(s, value)
  x
}

#' @export
print.support_product <- function(x, ...) {
  cat(sprintf(
    "<support: %s, %d variables>\n",
    vtype_of_support(x),
    support_dimension(x)
  ))
  pos <- 1L
  for (f in x[["factors"]]) {
    if (inherits(f, "support_points")) {
      cat(
        "-- (", paste(names(f[["points"]]), collapse = ", "), "): ",
        nrow(f[["points"]]), " points --\n",
        sep = ""
      )
    } else if (inherits(f, "support_map")) {
      cat(
        "-- (", paste(f[["variables"]], collapse = ", "), "): ",
        vtype_of_support(f), ", spanning ", f[["rank"]], " --\n",
        sep = ""
      )
    } else {
      cat("-- ", x[["variables"]][[pos]], ": ", sep = "")
      cat(format_univariate_support(f), "\n", sep = "")
    }
    pos <- pos + support_dimension(f)
  }
  invisible(x)
}

#' @export
print.support_points <- function(x, ...) {
  pts <- x[["points"]]
  cat(sprintf(
    "<support: discrete, %d variables, %d points>\n",
    ncol(pts),
    nrow(pts)
  ))
  print(pts[seq_len(min(6L, nrow(pts))), , drop = FALSE], row.names = FALSE)
  if (nrow(pts) > 6L) {
    cat("# ... with", nrow(pts) - 6L, "more points\n")
  }
  invisible(x)
}

# ---- internal helpers -------------------------------------------------------

#' Low-level constructor for a product support.
#' @param factors List of supports: univariate ones and point sets.
#' @param variables Character vector, one name per variable.
#' @noRd
new_support_product <- function(factors, variables) {
  structure(
    list(factors = factors, variables = variables),
    class = c("support_product", "support_mv", "support")
  )
}

#' Low-level constructor for a finite point set.
#' @param points A data frame of numeric columns, one per variable, with no
#' duplicated rows.
#' @noRd
new_support_points <- function(points) {
  structure(
    list(points = points),
    class = c("support_points", "support_mv", "support")
  )
}

#' Build a point-set support from a data frame or matrix of points.
#'
#' Called by `discrete()` when handed something with columns. A single column
#' is a univariate set of atoms, and comes back as an ordinary support.
#' @noRd
points_support <- function(x) {
  if (is.matrix(x)) {
    x <- as.data.frame(x)
    if (is.null(colnames(x)) || all(grepl("^V[0-9]+$", names(x)))) {
      names(x) <- rep("", ncol(x))
    }
  }
  if (!is.data.frame(x)) {
    stop("Points must be given as a data frame or a matrix.")
  }
  if (ncol(x) == 0) {
    stop("Points need at least one column, one per variable.")
  }
  ok <- vapply(x, is.numeric, logical(1))
  if (!all(ok)) {
    stop(
      "Every column of the points must be numeric.\n",
      "Column `", names(x)[!ok][[1L]], "` is not."
    )
  }
  if (anyNA(x)) {
    stop("Points must not contain `NA`.")
  }
  if (ncol(x) == 1L) {
    return(new_support(atoms = discretes::as_discretes(x[[1L]])))
  }
  names(x) <- fill_variable_names(rlang::names2(x))
  x <- unique(as.data.frame(lapply(x, as.numeric)))
  rownames(x) <- NULL
  new_support_points(x)
}

#' Give unnamed variables the names `x1`, `x2`, ... by position.
#' @noRd
fill_variable_names <- function(vars) {
  vars[is.na(vars)] <- ""
  blank <- vars == ""
  vars[blank] <- paste0("x", seq_along(vars))[blank]
  dup <- unique(vars[duplicated(vars)])
  if (length(dup) > 0) {
    stop(
      "Each variable needs its own name, but `", dup[[1L]],
      "` is used twice."
    )
  }
  vars
}

#' Rename the variables of a multivariate support, including the names held
#' by its pieces.
#' @noRd
rename_support <- function(s, value) {
  if (inherits(s, "support_points")) {
    names(s[["points"]]) <- value
    return(s)
  }
  if (inherits(s, "support_map")) {
    s[["variables"]] <- value
    names(s[["margins"]]) <- value
    if (!is.null(s[["shift"]])) {
      names(s[["shift"]]) <- value
    }
    return(s)
  }
  pos <- 1L
  for (k in seq_along(s[["factors"]])) {
    f <- s[["factors"]][[k]]
    d <- support_dimension(f)
    if (inherits(f, "support_mv")) {
      s[["factors"]][[k]] <- rename_support(f, value[pos:(pos + d - 1L)])
    }
    pos <- pos + d
  }
  s[["variables"]] <- value
  s
}

#' Names for the two variables of a `bi` constructor.
#'
#' The bivariate shortcuts fill unnamed variables with `x` and `y`, the names
#' of the `eval_bi_*()` arguments, rather than `x1` and `x2`.
#' @param nms Character vector of length 2 (or `NULL`), `""` for unnamed.
#' @noRd
bi_variable_names <- function(nms) {
  if (is.null(nms)) {
    nms <- c("", "")
  }
  nms[is.na(nms)] <- ""
  blank <- nms == ""
  nms[blank] <- c("x", "y")[blank]
  if (nms[[1L]] == nms[[2L]]) {
    stop(
      "Each variable needs its own name, but `", nms[[1L]],
      "` is used twice."
    )
  }
  nms
}

#' Number of variables in a support.
#' @noRd
support_dimension <- function(s) {
  if (inherits(s, "support_product") || inherits(s, "support_map")) {
    return(length(s[["variables"]]))
  }
  if (inherits(s, "support_points")) {
    return(ncol(s[["points"]]))
  }
  1L
}

#' Variable names of a support; NULL for a univariate one.
#' @noRd
support_variables <- function(s) {
  if (inherits(s, "support_product") || inherits(s, "support_map")) {
    return(s[["variables"]])
  }
  if (inherits(s, "support_points")) {
    return(names(s[["points"]]))
  }
  NULL
}

#' Is a support (or distribution) multivariate?
#' @noRd
is_multivariate <- function(x) {
  d <- dimension(x)
  !is.na(d) && d > 1L
}

#' A one-line description of a univariate support, for printing a product.
#' @noRd
format_univariate_support <- function(s) {
  parts <- character(0)
  n_atoms <- discretes::num_discretes(s[["atoms"]])
  if (n_atoms > 0) {
    if (is.finite(n_atoms) && n_atoms <= 6) {
      shown <- format(discretes::get_discretes_in(s[["atoms"]]))
      parts <- c(parts, paste0("{", paste(shown, collapse = ", "), "}"))
    } else {
      r <- range(s[["atoms"]])
      parts <- c(parts, sprintf(
        "%s atoms in [%g, %g]",
        if (is.finite(n_atoms)) n_atoms else "infinitely many",
        r[[1L]],
        r[[2L]]
      ))
    }
  }
  if (nrow(s[["continuous"]]) > 0) {
    ints <- apply(
      s[["continuous"]], 1L,
      function(r) sprintf("[%g, %g]", r[[1L]], r[[2L]])
    )
    parts <- c(parts, paste(ints, collapse = " U "))
  }
  if (length(parts) == 0) {
    return("empty")
  }
  paste(parts, collapse = " + ")
}

#' The variable type of a multivariate support.
#' @noRd
vtype_of_support_mv <- function(s) {
  if (inherits(s, "support_map")) {
    return(vtype_of_support_map(s))
  }
  if (inherits(s, "support_points")) {
    if (nrow(s[["points"]]) == 0) {
      return("empty")
    }
    return("discrete")
  }
  types <- vapply(s[["factors"]], vtype_of_support, character(1))
  if (any(types == "empty")) {
    return("empty")
  }
  if (all(types == "continuous")) {
    return("continuous")
  }
  # A lower-dimensional piece makes the whole product lower-dimensional.
  if (all(types %in% c("continuous", "singular"))) {
    return("singular")
  }
  if (all(types == "discrete")) {
    return("discrete")
  }
  "mixed"
}

#' Is a multivariate support empty?
#' @noRd
is_empty_support_mv <- function(s) {
  vtype_of_support_mv(s) == "empty"
}

#' The support of some of the variables.
#'
#' Projects a multivariate support onto the variables at positions `idx`,
#' keeping their order as given. A factor that is a point set is projected by
#' keeping the distinct rows of the chosen columns.
#' @param s A multivariate support.
#' @param idx Integer positions of the variables to keep.
#' @returns A support: univariate if `idx` has length 1.
#' @noRd
support_marginal <- function(s, idx) {
  if (inherits(s, "support_points")) {
    return(points_support(s[["points"]][idx]))
  }
  if (inherits(s, "support_map")) {
    return(support_marginal_map(s, idx))
  }
  factors <- s[["factors"]]
  dims <- vapply(factors, support_dimension, integer(1))
  owner <- rep(seq_along(factors), dims)
  within <- sequence(dims)
  # A multivariate piece pairs its variables; putting another variable
  # between two of them would need a product with interleaved pieces,
  # which a product cannot describe.
  runs <- owner[idx][c(TRUE, diff(owner[idx]) != 0)]
  split <- runs[duplicated(runs)]
  split <- split[vapply(factors[split], inherits, logical(1), "support_mv")]
  if (length(split) > 0L) {
    paired <- s[["variables"]][owner == split[[1L]]]
    stop(
      "The variables ", format_names(paired), " are paired in the\n",
      "support, and cannot be separated by another variable.\n",
      "Keep them next to each other.",
      call. = FALSE
    )
  }
  pieces <- list()
  piece_names <- character(0)
  # Walk the requested variables in order, grouping consecutive variables
  # that come from the same multivariate factor, so that how they are
  # paired within it is kept rather than crossed.
  k <- 1L
  while (k <= length(idx)) {
    f <- owner[[idx[[k]]]]
    run <- k
    while (run < length(idx) && owner[[idx[[run + 1L]]]] == f) {
      run <- run + 1L
    }
    fac <- factors[[f]]
    if (inherits(fac, "support_mv")) {
      piece <- support_marginal(fac, within[idx[k:run]])
      pieces <- c(pieces, list(piece))
      piece_names <- c(piece_names, if (run == k) {
        s[["variables"]][[idx[[k]]]]
      } else {
        ""
      })
    } else {
      pieces <- c(pieces, list(fac))
      piece_names <- c(piece_names, s[["variables"]][[idx[[k]]]])
    }
    k <- run + 1L
  }
  if (length(pieces) == 1L && length(idx) == 1L) {
    return(pieces[[1L]])
  }
  names(pieces) <- piece_names
  do.call(support_product, pieces)
}

#' List every point of a finite support, or NULL if it is not finite.
#'
#' A point set lists itself. A product lists every combination of its pieces,
#' provided each piece is finite and there are no more than `max_points`
#' combinations.
#' @returns A data frame with one column per variable, or `NULL`.
#' @noRd
enumerate_points <- function(s, max_points = 1e6) {
  if (inherits(s, "support_points")) {
    return(s[["points"]])
  }
  if (!inherits(s, "support_product")) {
    return(NULL)
  }
  frames <- list()
  for (f in s[["factors"]]) {
    if (inherits(f, "support_points")) {
      frames <- c(frames, list(f[["points"]]))
      next
    }
    if (inherits(f, "support_map")) {
      return(NULL)
    }
    if (nrow(f[["continuous"]]) > 0) {
      return(NULL)
    }
    n <- discretes::num_discretes(f[["atoms"]])
    if (!is.finite(n)) {
      return(NULL)
    }
    frames <- c(frames, list(data.frame(
      v = discretes::get_discretes_in(f[["atoms"]])
    )))
  }
  sizes <- vapply(frames, nrow, integer(1))
  if (prod(sizes) > max_points) {
    return(NULL)
  }
  grid <- expand.grid(lapply(sizes, seq_len), KEEP.OUT.ATTRS = FALSE)
  out <- do.call(cbind, lapply(seq_along(frames), function(j) {
    frames[[j]][grid[[j]], , drop = FALSE]
  }))
  out <- as.data.frame(out)
  names(out) <- s[["variables"]]
  rownames(out) <- NULL
  out
}
