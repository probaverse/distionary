#' LP3 distribution quantities
#'
#' p/d/q/r functions for the Log Pearson Type III distribution.
#' @param x,q Vector of quantiles/magnitudes.
#' @param n Number of observations to draw.
#' @param p Vector of probabilities.
#' @param meanlog,sdlog,skew Parameters.
#' @rdname LP3
#' @export
dlp3 <- function(x, meanlog, sdlog, skew) {
  retval <- dpearson3(log(x), meanlog, sdlog, skew) / x
  return(ifelse(x == 0, 0, retval))
}

#' @rdname LP3
#' @export
qlp3 <- function(p, meanlog, sdlog, skew) {
  exp(qpearson3(p, meanlog, sdlog, skew))
}

#' @rdname LP3
#' @export
qplp3 <- function(q, meanlog, sdlog, skew) {
  if (length(q) == 0) return(q)
  ifelse(
    q < 0, 0, ppearson3(log(q), meanlog, sdlog, skew)
  )
}

#' @rdname LP3
#' @export
rlp3 <- function(n, meanlog, sdlog, skew) {
  exp(rpearson3(n, meanlog, sdlog, skew))
}
