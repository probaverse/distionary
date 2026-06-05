#' Convert a data frame to a tibble
#'
#' Converts a data frame to a tibble, if the user has the `tibble`
#' package installed.
#' @param res Data frame.
#' @returns A tibble, if the user has the `tibble` package installed.
#' @noRd
convert_dataframe_to_tibble <- function(res) {
  checkmate::assert_data_frame(res)
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  res
}

#' Principal branch of the Lambert W function
#'
#' Solves `w * exp(w) = x` for `w` on the principal branch, defined for
#' `x >= -1 / e`. Used for distributions whose expectiles are expressed
#' through the Lambert W function, such as the Exponential.
#' @param x Numeric vector, each at least `-1 / e`.
#' @returns Numeric vector of the same length as `x`, the principal-branch
#' Lambert W values.
#' @noRd
lambert_w0 <- function(x) {
  w <- x
  near <- x < -0.3
  big <- x > 1
  p <- sqrt(pmax(2 * (exp(1) * x + 1), 0))
  w[near] <- -1 + p[near] - p[near]^2 / 3
  w[big] <- log(x[big]) - log(log(x[big]))
  w[x == 0] <- 0
  for (i in seq_len(50L)) {
    ew <- exp(w)
    f <- w * ew - x
    step <- f / (ew * (w + 1) - (w + 2) * f / (2 * w + 2))
    w <- w - step
    if (all(abs(step) < 1e-15, na.rm = TRUE)) {
      break
    }
  }
  w[is.infinite(x) & x > 0] <- Inf
  w
}
