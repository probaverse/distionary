#' Convert a data frame to a tibble
#'
#' Converts a data frame to a tibble, if the user has the `tibble`
#' package installed.
#' @param res Data frame.
#' @returns A tibble, if the user has the `tibble` package installed.
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @noRd
convert_dataframe_to_tibble <- function(res) {
  checkmate::assert_data_frame(res)
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  res
}
