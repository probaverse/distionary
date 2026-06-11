
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
#' @param .support The support of the distribution, built with [discrete()],
#' [continuous()], or [mixed()] (a bare `discretes` object is also accepted and
#' treated as `discrete()`). The variable type ([vtype()]) is derived from it.
#' Preferred over `.vtype`.
#' @param .vtype `r lifecycle::badge("superseded")` Superseded by `.support`.
#' The variable type, typically "discrete" or "continuous"; a length-1
#' character vector, converted to lowercase with `tolower()` for compliance
#' with known types.
#' @param .name A name to give to the distribution.
#' Can be any character vector of length 1.
#' @param .parameters A named list with one entry per distribution parameter,
#' each of which can be any data type. In this version of distionary,
#' these parameters are only stored for the benefit of the user to know
#' what distribution they are working with; the code never looks at these
#' parameters to inform its calculations. This is anticipated to change in
#' a future version of distionary.
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
#'   .support = continuous(c(0, 1)),
#'   .name = "My Linear",
#'   .parameters = list(could = "include", anything = data.frame(x = 1:10))
#' )
#'
#' # Inspect
#' linear
#'
#' # Plot
#' plot(linear)
#' @family Distribution Construction
#' @export
distribution <- function(...,
                         .support = NULL,
                         .vtype = NULL,
                         .name = NULL,
                         .parameters = list()) {
  support <- NULL
  if (!is.null(.support)) {
    support <- as_support(.support)
  }
  # Derive the variable type. From the support when we have one; otherwise from
  # the legacy `.vtype` string (status quo, including typo detection).
  if (!is.null(support)) {
    .vtype <- vtype_of_support(support)
  } else if (!is.null(.vtype)) {
    lifecycle::deprecate_soft(
      when = "0.2.0",
      what = "distribution(.vtype)",
      with = "distribution(.support)"
    )
    if (is_support(.vtype) || inherits(.vtype, "discretes")) {
      stop(
        "`.vtype` accepts only a character variable type. ",
        "Pass support objects to `.support` instead."
      )
    }
    .vtype <- tolower(as.character(.vtype))
    checkmate::assert_character(.vtype, len = 1)
    # Typo detection for variable type.
    vtypes <- c("discrete", "continuous", "ordinal", "categorical", "mixed")
    vtype_match <- agrep(.vtype, vtypes, max.distance = 0.1, value = TRUE)
    if (length(vtype_match) > 0 && !(.vtype %in% vtype_match)) {
      warning(paste0(
        "The .vtype '", .vtype, "' looks similar to ",
        paste(vtype_match, collapse = ", "), "."
      ))
    }
  } else {
    .vtype <- "unknown"
  }
  if (!is.null(.name)) {
    .name <- as.character(.name)
  } else {
    .name <- "Unnamed"
  }
  checkmate::assert_character(.name, len = 1, null.ok = TRUE)
  checkmate::assert_list(.parameters, names = "named", null.ok = TRUE)
  dots <- rlang::enquos(...)
  checkmate::assert_list(dots, names = "named", null.ok = TRUE)
  representations <- lapply(dots, rlang::eval_tidy)
  # Check for required properties.
  reps_missing <- is.null(representations$cdf) ||
    (is.null(representations$density) && is.null(representations$pmf))
  if (reps_missing) {
    warning(
      "Full suite of distribution properties may not be accessible ",
      "without specifying 'cdf', and either 'density' or 'pmf'."
    )
  }
  new_distribution(
    representations,
    vtype = .vtype, name = .name, parameters = .parameters, support = support
  )
}
