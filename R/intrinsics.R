#' Intrinsically defined distribution properties
#'
#' Get the names of intrinsically defined distribution properties
#' (`intrinsics()`), or check if a property is intrinsically defined
#' (`is_intrinsic()`).
#' @param distribution The distribution to check.
#' @param property Name of the property to check; character vector.
#' @returns For `intrinsics()`, a character vector of property names
#' that have been intrinsically defined in the distribution.
#'
#' For `is_intrinsic()`, a logical vector of length equal to the length
#' of `property` indicating whether each property in `property`
#' is intrinsically defined in the distribution
#' (`TRUE`) or not (`FALSE`). If `property` is NA, the output is `NA`.
#' @noRd
intrinsics <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  names(distribution)
}

#' @noRd
is_intrinsic <- function(distribution, property) {
  checkmate::assert_class(distribution, "dst")
  property <- as.character(property)
  checkmate::assert_character(property)
  res <- property %in% intrinsics(distribution)
  res[is.na(property)] <- NA
  res
}
