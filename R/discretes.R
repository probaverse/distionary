#' Probing discrete values in a distribution
#'
#' `next_discrete()` and `prev_discrete()` find the `n` discrete values
#' in a distribution next to a reference point. `num_discretes()` finds
#' the number of discrete values within a range. `has_infinite_discretes()`
#' checks whether there are an infinite amount of discrete values between
#' a range of values.
#'
#' @param distribution Distribution
#' @param from,to Reference values.
#' @param n Number of discrete values to find.
#' @param include_from,include_to Logical; should the `from` value be included
#' in the query? Should the `to` value?
#' @return For `next_discrete()` and `prev_discrete()`, a vector of
#' all available discrete points satisfying the query.
#' If less values are available than asked
#' via `n`, only those values are returned.
#' If infinite values satisfy the query, an error is thrown;
#' `NaN` occurs when no one particular discrete value follows, such as
#' when asking for the integer that comes before infinity.
#'
#' For `num_discretes()`, a single non-negative integer, possibly infinite.
#' Possibly also `NA_integer_` if there's not enough information to
#' determine this.
#'
#' For `has_infinite_discretes()`, a single logical, possibly `NA` if
#' there's not enough information to determine this.
#' @examples
#' next_discrete(dst_pois(1), from = 1.3)
#' prev_discrete(dst_pois(1), from = 3, n = 10)
#' next_discrete(dst_norm(0, 1), from = 1.3, n = 4)
#' @rdname discretes
#' @export
next_discrete <- function(distribution, from, n = 1L, include_from = FALSE) {
  UseMethod("next_discrete")
}

#' @export
#' @inheritParams next_discrete
next_discrete.dst <- function(distribution, from, n, include_from) {
  numeric(0L)
}

#' @rdname discretes
#' @export
prev_discrete <- function(distribution, from, n = 1L, include_from = FALSE) {
  UseMethod("prev_discrete")
}

#' @export
#' @inheritParams next_discrete
prev_discrete.dst <- function(distribution, from, n, include_from) {
  numeric(0L)
}

#' @rdname discretes
#' @export
num_discretes <- function(distribution, from, to, include_from, include_to) {
  UseMethod("num_discretes")
}

#' @export
num_discretes.dst <- function(distribution, from, to,
                              include_from, include_to) {
  if (vtype(distribution) == "continuous") {
    return(0L)
  }
  NA_integer_
}

#' @rdname discretes
#' @export
has_infinite_discretes <- function(distribution, from = -Inf, to = Inf) {
  UseMethod("has_infinite_discretes")
}

#' @export
has_infinite_discretes.dst <- function(distribution, from, to) {
  if (vtype(distribution) == "continuous") {
    return(FALSE)
  }
  NA
}



#' Helper functions for finding discrete values
#'
#' `*_discrete_finite` finds discrete values from a
#' finite set of possibilities. `*_discret_natural` finds
#' discrete values amongst the natural numbers (including 0).
#'
#' @param x A vector of all possible discrete values.
#' @param from Reference value.
#' @param include_from Logical; should the `from` value be inclusive?
#' @inheritParams next_discrete
#' @rdname discrete_helpers
next_discrete_finite <- function(x, from, n, include_from) {
  if (include_from) {
    query <- x >= from
  } else {
    query <- x > from
  }
  higher_discretes <- x[query]
  n <- min(length(higher_discretes), n)
  sort(higher_discretes)[seq_len(n)]
}

#' @rdname discrete_helpers
prev_discrete_finite <- function(x, from, n, include_from) {
  if (include_from) {
    query <- x <= from
  } else {
    query <- x < from
  }
  lower_discretes <- x[query]
  n <- min(length(lower_discretes), n)
  sort(lower_discretes, decreasing = TRUE)[seq_len(n)]
}

#' @rdname discrete_helpers
next_discrete_natural <- function(from, n, include_from) {
  if (from == Inf) {
    return(numeric(0L))
  }
  if (is.infinite(n)) {
    stop("Your selection includes an infinite number of discrete points.")
  }
  if (from < 0) {
    return(seq_len(n) - 1L)
  }
  floor_from <- floor(from)
  adjust <- from == floor_from && include_from
  floor_from + seq_len(n) - adjust
}

#' @rdname discrete_helpers
prev_discrete_natural <- function(from, n, include_from) {
  if (n > 0 && from == Inf) {
    return(NaN)
  }
  ceil_from <- ceiling(from)
  adjust <- from == ceil_from && include_from
  n_max <- max(ceil_from + adjust, 0)
  n <- min(n, n_max)
  ceil_from - seq_len(n) + adjust
}
