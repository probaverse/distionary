eval_stdev_from_network <- function(distribution) {
  ss <- variance(distribution)
  sqrt(ss)
}
