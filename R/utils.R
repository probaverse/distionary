convert_dataframe_to_tibble <- function(res) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  res
}
