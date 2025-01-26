#' Make a distribution
#'
#' Make a distribution object by specifying its representations
#' (e.g., cdf, density, mean, etc.).
#' Some representations, if not included, will be calculated based
#' on other representations that are included (e.g., quantile from cdf).
#' A list of these representations can be found in the details.
#' Note that any object can be added to the distribution.
#'
#' @param ... Name-value pairs to add to the distribution list.
#' @param .vtype The variable type, indicating the values for which
#' the distribuion provides probabilities for. For example,.
#' `"continuous"` implies that the distribution acts on real numbers;
#' `"discrete"` implies discrete values.
#' @param .name If this distribution has a name (e.g., "Normal"),
#' optionally specify it here.
#' @return A distribution object.
#' @details
#' Currently, at least two representations are required to be specified if
#' you want to be able to retrieve other properties without specifying them:
#'
#' - `cdf`: the cumulative distribution function.
#' - `density`, the probability density function for continuous variables,
#'   or `pmf`, the probability mass function for discrete variables.
#'
#' Other representations that will be retrieved if missing include:
#'
#' - `survival`: the survival function, or one minus the cdf.
#' - `hazard`: the hazard function, for continuous variables only.
#' - `chf`: the cumulative hazard function, for continuous variables only.
#' - `quantile`: the quantile function, or left-inverse of the cdf.
#' - `realise` or `realize`: a function that takes an integer and generates
#'   a vector of that many random draws from the distribution.
#' - `odds`: for discrete variables, the probability odds function
#'   (pmf / (1 - pmf))
#' - `return`: the quantiles associated with the provided return periods,
#'   where events are exceedances.
#'
#' All functions should be vectorized.
#'
#' Properties that can be retrieved if not specified include the following:
#'
#' - `mean`, `stdev`, `variance`, `skewness`, `median` are self-explanatory.
#' - `kurtosis_exc` and `kurtosis` are the distribution's excess
#'   kurtosis and regular kurtosis.
#' - `range`: A vector of the minimum and maximum value of a distribution's
#'   support.
#'
#' A later version of distionary will allow for the specification of
#' custom representations / properties.
#' @export
distribution <- function(..., .vtype = NULL, .name = NULL) {
  dots <- rlang::enquos(...)
  representations <- lapply(dots, rlang::eval_tidy)
  new_distribution(representations, vtype = .vtype, name = .name)
}
