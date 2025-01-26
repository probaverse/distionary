#' Representations of the Generalized Pareto Distribution
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param lower.tail Logical. If `TRUE`, cdf (default);
#' if `FALSE`, survival function.
#' @examples
#' pgpd(1:10, 0, 1, 1)
#' dgpd(1:10, 0, 2, 0)
#' qgpd(1:9 / 10, 2, 10, -2)
#' @rdname gpd_raw
#' @inheritParams dst_gpd
#' @export
pgpd <- function(q, location, scale, shape, lower.tail = TRUE) {
  if (shape == 0) {
    left <- q < location
    z <- (q - location) / scale
    if (lower.tail) {
      res <- 1 - exp(-z)
      res[left] <- 0
    } else {
      res <- exp(-z)
      res[left] <- 1
    }
    res
  } else {
    if (shape > 0) {
      rightend <- Inf
    } else {
      rightend <- location - scale / shape
    }
    left <- q < location
    right <- q > rightend
    z <- (q - location) / scale
    if (lower.tail) {
      res <- 1 - (1 + shape * z)^(-1 / shape)
      res[left] <- 0
      res[right] <- 1
    } else {
      res <- (1 + shape * z)^(-1 / shape)
      res[left] <- 1
      res[right] <- 0
    }
    res
  }
}


#' @rdname gpd_raw
#' @inheritParams pgpd
#' @export
qgpd <- function(p, location, scale, shape) {
  invalid <- p < 0 | p > 1
  if (shape == 0) {
    res <- location - scale * log(1 - p)
    res[invalid] <- NaN
    res
  } else {
    if (shape > 0) {
      rightend <- Inf
    } else {
      rightend <- location - scale / shape
    }
    t <- 1 / (1 - p)
    res <- location + scale * (t^shape - 1) / shape
    res[invalid] <- NaN
    res
  }
}

#' @rdname gpd_raw
#' @inheritParams pgpd
#' @export
dgpd <- function(x, location, scale, shape) {
  z <- (x - location) / scale
  if (shape == 0) {
    outside <- x < location
    res <- exp(-z) / scale
    res[outside] <- 0
    res
  } else {
    if (shape > 0) {
      rightend <- Inf
    } else {
      rightend <- location - scale / shape
    }
    outside <- x < location | x > rightend
    res <- (1 + shape * z)^(-1 / shape - 1) / scale
    res[outside] <- 0
    res
  }
}

