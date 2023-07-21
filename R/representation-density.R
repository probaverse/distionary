#' @include new_enframe.R
NULL

#' @rdname eval_uni
#' @export
eval_density <- function(distribution, at, strict = TRUE, ...) {
  UseMethod("eval_density")
}

#' @rdname eval_multi
#' @export
eval_bi_density <- function(distribution, x, y, strict = TRUE, ...) {
  UseMethod("eval_bi_density")
}

#' @rdname eval_multi
#' @export
eval_multi_density <- function(distribution, .l, strict = TRUE, ...) {
  UseMethod("eval_multi_density")
}


#' @export
enframe_density <- new_enframe_uni("density")

#' @export
enframe_bi_density <- new_enframe_bi("density")

#' @export
enframe_multi_density <- new_enframe_multi("density")
