#' @noRd
eval_range_from_network <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  support <- attributes(distribution)[["support"]]
  if (!is.null(support)) {
    hull <- support_hull(support)
    if (all(!is.na(hull))) {
      return(hull)
    }
  }
  eval_quantile(distribution, at = 0:1)
}
