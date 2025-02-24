#' @noRd
eval_stdev_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  ss <- variance(distribution)
  sqrt(ss)
}
