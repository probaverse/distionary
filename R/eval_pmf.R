#' @export
eval_pmf.dst <- function(distribution, at, strict = TRUE) {
  if (variable(distribution) == "discrete") {
    stop("Cannot find the pmf for this distribution.")
  }
  if (strict) {
    stop("This distribution does not have a pmf. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (variable(distribution) == "continuous") {
      return(rep(0, length(at)))
    } else {
      stop("Cannot find probabilities for this distribution.")
    }
  }
}

