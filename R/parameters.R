#' Parameters of a Distribution
#'
#' Retrieve the parameters of a distribution, if applicable. See
#' details.
#'
#' @param distribution Distribution.
#' @details If a distribution is made by specifying parameter values
#' (e.g., mean and variance for a Normal distribution; shape parameters
#' for a Beta distribution), it is useful to keep track of what
#' these parameters are. This is done by adding `parameters`
#' to the list of objects defining the distribution; for instance,
#' `distribution(parameters = c(shape1 = 1.4, shape2 = 3.4))`.
#' Note that no checks are made to ensure the parameters are valid.
#' It's important to note that, in this version of distionary,
#' manually changing the parameters after the distribution has been
#' created will not change the functionality of the distribution.
#' @returns The `"parameters"` entry of the list making up the
#' probability distribution.
#' @export
parameters <- function(distribution) {
  distribution[["parameters"]]
}

`parameters<-` <- function(distribution, value) {
  distribution[["parameters"]] <- value
  distribution
}
