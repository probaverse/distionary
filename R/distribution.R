#' Build a Distribution Object
#'
#' Make a distribution object by specifying properties
#' (e.g., cdf, density, mean, etc.).
#' Some properties, if not included, will be calculated based
#' on other properties that are included (e.g., quantile from cdf;
#' variance from standard deviation).
#' A list of these representations can be found in the details.
#'
#' @param ... Name-value pairs for defining the distribution.
#' @param .vtype The variable type, typically "discrete" or "continuous".
#' Can be any character vector of length 1; if not a character, is
#' converted to one with `as.character()`.
#' @param .name A name to give to the distribution.
#' Can be any character vector of length 1; if not a character, is
#' converted to one with `as.character()`.
#' @return A distribution object.
#' @details
#' Currently, the CDF (`cdf`) is required to be specified, along with the PMF
#' (`pmf`) for discrete distributions and density (`density`) for continuous
#' distributions. Otherwise, the full extent of distribution properties will
#' not be accessible.
#'
#' A distributional representation is a function that fully describes the
#' distribution. Besides `cdf`, `density`, and `pmf`, other options
#' understood by `distionary` include:
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
#' Other properties that are understood by `distionary` include:
#'
#' - `mean`, `stdev`, `variance`, `skewness`, `median` are self-explanatory.
#' - `kurtosis_exc` and `kurtosis` are the distribution's excess
#'   kurtosis and regular kurtosis.
#' - `range`: A vector of the minimum and maximum value of a distribution's
#'   support.
#' @examples
#' linear <- distribution(
#'   density = function(x) {
#'     d <- 2 * (1 - x)
#'     d[x < 0 | x > 1] <- 0
#'     d
#'   },
#'   cdf = function(x) {
#'     p <- 2 * x * (1 - x / 2)
#'     p[x < 0] <- 0
#'     p[x > 1] <- 1
#'     p
#'   },
#'   .vtype = "continuous",
#'   .name = "My Linear"
#' )
#'
#' # Inspect
#' linear
#'
#' # Plot
#' plot(linear)
#' @family Distribution Construction
#' @export
distribution <- function(..., .vtype = NULL, .name = NULL) {
  dots <- rlang::enquos(...)
  if (!is.null(.vtype) && length(.vtype) != 1) {
    stop("Only one variable type allowed.")
  }
  if (!is.null(.name) && length(.name) != 1) {
    stop("Only one distribution name allowed.")
  }
  if (is.null(.name)) {
    .name <- "Unnamed"
  }
  if (!is.null(.vtype)) {
    .vtype <- as.character(.vtype)
  }
  .name <- as.character(.name)
  representations <- lapply(dots, rlang::eval_tidy)
  new_distribution(representations, vtype = .vtype, name = .name)
}
