#' Constructor Function for "dst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#'   or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_distribution <- function(l, variable, ...,
                             class = character()) {
  structure(
    l,
    variable = variable,
    class    = c(class, "dst")
  )
}


#' Distribution Objects
#' @param object Object to be tested
#' @rdname distribution
#' @export
is_distribution <- function(object) {
  inherits(object, "dst") || inherits(object, "multidst")
}

#' @rdname distribution
#' @export
is.distribution <- is_distribution

#' Make a blank distribution
#'
#' Currently, this function makes a distribution object with nothing in it. The
#' idea is that you can then set things downstream, with functions such as
#' set_cdf() and set_mean(). The idea behind this function is expected to be in
#' flux.
#'
#' @param variable Is this variable continuous, discrete, or mixed?
#' @return A distribution object with nothing in it.
#' @export
distribution <- function(variable = c("continuous", "discrete", "mixed")) {
  variable <- match.arg(variable)
  new_distribution(list(), variable = variable)
}
