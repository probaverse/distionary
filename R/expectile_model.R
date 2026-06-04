#' Expectile-based Distribution Model
#'
#' @description
#' Constructs a distribution using an expectile function.
#'
#' @details
#' The distribution is reconstructed through:
#'
#'   φ(x) = E[(X - x)^+]
#'
#' and:
#'
#'   survival(x) = -φ'(x)
#'
#' This allows recovery of:
#' - CDF
#' - survival
#' - discrete mass
#'
#' @param tau Expectile levels (increasing)
#' @param xi Expectile values
#' @param mean Mean of the distribution
#'
#' @return An object of class "expectile_model"
#' @export
expectile_model <- function(tau, xi, mean) {

  ptr <- new(ExpectileModel, tau, xi, mean)

  structure(
    list(ptr = ptr),
    class = "expectile_model"
  )
}