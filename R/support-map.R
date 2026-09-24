#' A Support as the Image of Another
#'
#' Describe a support as where a map sends a simpler one. This is how to
#' describe supports that are not products, such as a triangle (`y <= x`) or
#' a line (`x + y = 100`): each is the image of a product under a map.
#' `support_affine()` is for linear maps, where everything about the image
#' can be worked out exactly; `support_map()` is for any map, and needs to be
#' told what cannot be worked out.
#'
#' @param support The support being mapped: univariate, or a product of
#' univariate supports.
#' @param fun For `support_map()`, the map. It takes one vector per variable
#' of `support` (as the representations of a multivariate distribution do)
#' and returns a list of vectors, one per output variable.
#' @param ... Not used; forces the other arguments to be named.
#' @param margins For `support_map()`, a list of univariate supports, one per
#' output variable: the values each can take. They cannot, in general, be
#' worked out from `fun`, so they are stated. Their names name the variables.
#' @param shift,matrix For `support_affine()`, the map
#' \eqn{u \mapsto \mathrm{shift} + \mathrm{matrix} \, u}. `matrix` has one row
#' per output variable and one column per variable of `support`. The names
#' of `shift`, or the row names of `matrix`, name the variables.
#' @details
#' ## Why describe a support by a map
#'
#' The alternative is a product with restrictions laid on it: all of
#' \eqn{(x, y)}, subject to \eqn{y \le x}. That is easy to state, but a
#' restriction can only answer whether a point belongs; it cannot say what
#' values one variable reaches, or how many dimensions the support spans.
#' A map says both. The support of \eqn{(X, XZ)}, for \eqn{X > 0} and
#' \eqn{0 < Z < 1}, is the image of a rectangle under
#' \eqn{(x, z) \mapsto (x, xz)}; the line \eqn{x + y = 100} is the image of
#' the real line under \eqn{t \mapsto (50 + t, 50 - t)}.
#'
#' ## Dimension and variable type
#'
#' The support has one variable per output of the map ([dimension()]), but
#' may span fewer dimensions than that: a line in the plane spans one. When
#' it spans fewer, its variable type ([vtype()]) is `"singular"`. There is
#' then no density with respect to area (or volume), even though no single
#' point carries any probability. Take a [marginal()] to get back a
#' distribution with a density.
#'
#' For `support_affine()`, the number of dimensions spanned is the rank of
#' `matrix`. For `support_map()`, it is taken to be the smaller of the
#' number of variables going in and coming out.
#'
#' ## Restrictions
#'
#' `support_affine()` works out each variable's support exactly, which needs
#' `support` to be continuous: for a finite set of points it gives the
#' mapped points directly, and anything else with atoms is not supported
#' yet.
#' @returns A support object.
#' @seealso [support_product()] for supports that are products.
#' @examples
#' # The line x + y = 100, as the image of the real line.
#' line <- support_affine(
#'   continuous(),
#'   shift = c(x = 50, y = 50),
#'   matrix = rbind(1, -1)
#' )
#' line
#' dimension(line)
#'
#' # The triangle 0 < y < x, as the image of (x, z) -> (x, x * z).
#' support_map(
#'   support_product(x = continuous(c(0, Inf)), z = continuous(c(0, 1))),
#'   fun = function(x, z) list(x, x * z),
#'   margins = list(x = continuous(c(0, Inf)), y = continuous(c(0, Inf)))
#' )
#' @family Support
#' @export
support_map <- function(support, fun, ..., margins) {
  rlang::check_dots_empty()
  base <- as_support(support)
  checkmate::assert_function(fun)
  if (!is.list(margins) || length(margins) == 0) {
    stop("`margins` must be a list of supports, one per variable.")
  }
  margins <- lapply(margins, as_support)
  for (m in margins) {
    assert_univariate_support(m, "support_map")
  }
  vars <- fill_variable_names(rlang::names2(margins))
  names(margins) <- vars
  if (length(margins) == 1L) {
    return(margins[[1L]])
  }
  rank <- min(support_dimension(base), length(margins))
  new_support_map(base, fun, margins, vars, rank)
}

#' @rdname support_map
#' @export
support_affine <- function(support, ..., shift, matrix) {
  rlang::check_dots_empty()
  base <- as_support(support)
  checkmate::assert_numeric(shift, any.missing = FALSE, min.len = 1)
  if (!is.matrix(matrix)) {
    matrix <- as.matrix(matrix)
  }
  checkmate::assert_numeric(matrix, any.missing = FALSE)
  r <- support_dimension(base)
  p <- length(shift)
  if (nrow(matrix) != p || ncol(matrix) != r) {
    stop(
      "`matrix` must have one row per entry of `shift` (", p, ")\n",
      "and one column per variable of `support` (", r, ")."
    )
  }
  vars <- names(shift)
  if (is.null(vars)) {
    vars <- rownames(matrix)
  }
  vars <- fill_variable_names(if (is.null(vars)) rep("", p) else vars)
  shift <- stats::setNames(as.numeric(shift), vars)
  matrix <- unname(matrix)
  # Finitely many points map to finitely many points.
  pts <- enumerate_points(base)
  if (is.null(pts) && !inherits(base, "support_mv") &&
    is.finite(discretes::num_discretes(base[["atoms"]])) &&
    nrow(base[["continuous"]]) == 0) {
    pts <- data.frame(u = discretes::get_discretes_in(base[["atoms"]]))
  }
  if (!is.null(pts)) {
    image <- as.data.frame(t(shift + matrix %*% t(as.matrix(pts))))
    names(image) <- vars
    return(discrete(image))
  }
  if (!all(affine_base_types(base) == "continuous")) {
    stop(
      "`support_affine()` needs `support` to be continuous, or finitely\n",
      "many points. Supports with infinitely many atoms are not\n",
      "supported yet."
    )
  }
  margins <- lapply(seq_len(p), function(i) {
    affine_margin(base, shift[[i]], matrix[i, ])
  })
  names(margins) <- vars
  if (p == 1L) {
    return(margins[[1L]])
  }
  fun <- function(...) {
    u <- do.call(cbind, vctrs::vec_recycle_common(...))
    out <- t(shift + matrix %*% t(u))
    stats::setNames(lapply(seq_len(p), function(i) out[, i]), vars)
  }
  rank <- qr(matrix)$rank
  s <- new_support_map(base, fun, margins, vars, rank)
  s[["shift"]] <- shift
  s[["matrix"]] <- matrix
  class(s) <- c("support_affine", class(s))
  s
}

#' @export
print.support_map <- function(x, ...) {
  p <- length(x[["variables"]])
  cat(sprintf(
    "<support: %s, %d variables spanning %d dimension%s>\n",
    vtype_of_support(x),
    p,
    x[["rank"]],
    if (x[["rank"]] == 1L) "" else "s"
  ))
  kind <- if (inherits(x, "support_affine")) "an affine" else "a"
  cat("-- the image of", kind, "map; each variable reaches:\n")
  for (v in x[["variables"]]) {
    cat(
      "-- ", v, ": ", format_univariate_support(x[["margins"]][[v]]), "\n",
      sep = ""
    )
  }
  invisible(x)
}

# ---- internal helpers -------------------------------------------------------

#' Low-level constructor for a mapped support.
#' @param base The support being mapped.
#' @param fun The map.
#' @param margins Named list of univariate supports, one per output.
#' @param variables Output variable names.
#' @param rank Number of dimensions the image spans.
#' @noRd
new_support_map <- function(base, fun, margins, variables, rank) {
  structure(
    list(
      base = base,
      fun = fun,
      margins = margins,
      variables = variables,
      rank = as.integer(rank)
    ),
    class = c("support_map", "support_mv", "support")
  )
}

#' Variable types of the univariate pieces of a support to be mapped.
#' @noRd
affine_base_types <- function(base) {
  if (inherits(base, "support_product")) {
    return(vapply(base[["factors"]], vtype_of_support, character(1)))
  }
  if (inherits(base, "support_mv")) {
    return("other")
  }
  vtype_of_support(base)
}

#' The univariate supports of a continuous support's variables, in order.
#' @noRd
base_margins <- function(base) {
  if (inherits(base, "support_product")) {
    return(base[["factors"]])
  }
  list(base)
}

#' Where `shift + sum(coefs * u)` reaches as `u` ranges over a continuous
#' product: the scaled regions of each variable, added together.
#' @noRd
affine_margin <- function(base, shift, coefs) {
  pieces <- base_margins(base)
  acc <- matrix(c(shift, shift), ncol = 2L)
  for (j in seq_along(coefs)) {
    a <- coefs[[j]]
    if (a == 0) {
      next
    }
    reg <- pieces[[j]][["continuous"]] * a
    if (a < 0) {
      reg <- reg[, 2:1, drop = FALSE]
    }
    # Every region so far, plus every region of this variable.
    idx <- expand.grid(i = seq_len(nrow(acc)), k = seq_len(nrow(reg)))
    acc <- cbind(
      acc[idx$i, 1L] + reg[idx$k, 1L],
      acc[idx$i, 2L] + reg[idx$k, 2L]
    )
  }
  if (all(coefs == 0)) {
    return(discrete(shift))
  }
  continuous(acc)
}

#' Project a mapped support onto some of its variables.
#' @noRd
support_marginal_map <- function(s, idx) {
  if (length(idx) == 1L) {
    return(s[["margins"]][[idx]])
  }
  vars <- s[["variables"]][idx]
  if (inherits(s, "support_affine")) {
    return(support_affine(
      s[["base"]],
      shift = s[["shift"]][idx],
      matrix = s[["matrix"]][idx, , drop = FALSE]
    ))
  }
  fun <- s[["fun"]]
  support_map(
    s[["base"]],
    fun = function(...) fun(...)[idx],
    margins = s[["margins"]][idx]
  )
}

#' The variable type of a mapped support.
#' @noRd
vtype_of_support_map <- function(s) {
  base_type <- vtype_of_support(s[["base"]])
  if (base_type %in% c("continuous", "singular") &&
    s[["rank"]] < length(s[["variables"]])) {
    return("singular")
  }
  base_type
}
