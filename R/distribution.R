#' @srrstats {G1.0} Distributions are generic enough to not need a specific
#' reference (e.g., an intro probability book will cover most concepts in
#' distionary), and it appears most other distribution packages on CRAN do
#' not include references.
#' @srrstats {G1.4} Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to
#' document all functions.
#' @srrstats {G1.4a} All internal (non-exported) functions are documented in
#' standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a
#' final `@noRd` tag to suppress automatic generation of `.Rd` files.
#' @srrstats {PD1.0} Distributions are treated generally in distionary.
#' @srrstats {PD2.0} Representing probability distributions using a package
#' for general representation is the main purpose of distionary.

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
#' Can be any character vector of length 1, but is converted to
#' lowercase with `tolower()` for compliance with known types.
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
#'   .vtype = "continuous",
#'   .name = "My Linear",
#'   .parameters = list(could = "include", anything = data.frame(x = 1:10))
#' )
#'
#' # Inspect
#' linear
#'
#' # Plot
#' plot(linear)
#' @srrstats {G2.3} Univariate character input specifications are asserted
#' using the checkmate package where relevant (e.g., `.vtype` and `.name`
#' in `distribution()`; `arg_name` and `fn_prefix` in `enframe_*()`).
#' --> Copied to those functions.
#' @srrstats {G2.3b} The use of `tolower()` is applicable for the `.name`
#' argument in `distribution()` and is used. --> Copied to `distribution()`.
#' @srrstats {G2.4c} Explicit conversion to character via `as.character()`
#' (and not `paste` or `paste0`) is done where character input is required:
#' `distribution()`'s `.vtype` and `.name` arguments, and the column naming
#' specifications of `enframe_general()`. --> Copied to both functions.
#' @family Distribution Construction
#' @export
distribution <- function(...,
                         .vtype = NULL,
                         .name = NULL,
                         .parameters = list()) {
  if (!is.null(.vtype)) {
    .vtype <- as.character(.vtype)
  } else {
    .vtype <- "unknown"
  }
  if (!is.null(.name)) {
    .name <- as.character(.name)
  } else {
    .name <- "Unnamed"
  }
  checkmate::assert_character(.vtype, len = 1, null.ok = TRUE)
  checkmate::assert_character(.name, len = 1, null.ok = TRUE)
  checkmate::assert_list(.parameters, names = "named", null.ok = TRUE)
  .vtype <- tolower(.vtype)
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
  # Typo detection for variable type.
  vtypes <- c("discrete", "continuous", "ordinal", "categorical", "mixed")
  vtype_match <- agrep(.vtype, vtypes, max.distance = 0.1, value = TRUE)
  if (length(vtype_match) > 0 && !(.vtype %in% vtype_match)) {
    warning(paste0(
      "The .vtype '", .vtype, "' looks similar to ",
      paste(vtype_match, collapse = ", "), "."
    ))
  }
  new_distribution(
    representations,
    vtype = .vtype, name = .name, parameters = .parameters
  )
}
