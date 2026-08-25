
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
#' @param .support **Required.** Where the distribution places probability,
#' built with [discrete()], [continuous()] or [mixed()]. A bare `discretes`
#' object is also accepted, and treated as [discrete()]. See Details.
#' @param .vtype `r lifecycle::badge("defunct")` Removed in favour of
#' `.support`, and now an error. See Details.
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
#' ## The support
#'
#' Every distribution has to say where it places probability, and `.support`
#' is how. It is the one thing distionary asks for rather than working out: a
#' CDF does hold the answer, its jumps being the atoms and its flattening out
#' marking where the distribution ends, but recovering that numerically means
#' hunting for discontinuities in a function that can only be sampled. The
#' estimate would be worst for small atoms and long tails, which are the cases
#' where it matters most.
#'
#' Declared instead, it is exact, and the difference shows: quantiles at
#' probability 0 and 1 are read off rather than searched for in the numerical
#' tail, atoms are located exactly, and moments can be decomposed. It is the
#' same bargain as declaring atoms --- a little more to say up front, in
#' exchange for exact answers rather than approximate ones. The
#' "The Support of a Distribution" vignette covers what a support is and how
#' to build one.
#'
#' The variable type ([vtype()]) and the [range()] follow from the support, so
#' neither can be given here; see the property list below.
#'
#' `.vtype` used to take a string such as `"continuous"` and is now defunct. A
#' variable type cannot stand in for a support: `"discrete"` does not say
#' *which* points carry mass, and `"continuous"` does not say over what
#' region, so there is no translating one into the other, and a guess would be
#' quietly wrong rather than an error. The argument is kept only so that old
#' code gets a message saying what to do, rather than `unused argument`.
#'
#' ## Properties
#'
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
#' A representation given as a plain function provides that representation in
#' its canonical variant: `quantile` is the left inverse of the CDF, `return`
#' gives the levels of exceedance events. To provide more than that --- the
#' right inverse as well as the left, say --- wrap the functions with
#' [variants()].
#'
#' Other properties that are understood by `distionary` include:
#'
#' - `mean`, `stdev`, `variance`, `skewness`, `median` are self-explanatory.
#' - `kurtosis_exc` and `kurtosis` are the distribution's excess
#'   kurtosis and regular kurtosis.
#'
#' `range` and `vtype` are properties too, and [eval_property()] reads them
#' like any other, but they cannot be given here: the support determines both,
#' and a stated one could disagree with it. A name distionary does not know is
#' simply kept, retrievable with [eval_property()] and otherwise unused.
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
distribution <- function(
  ...,
  .support = NULL,
  .vtype = NULL,
  .name = NULL,
  .parameters = list()
) {
  # Checked before the support, so that old code passing `.vtype` gets the
  # message naming its replacement rather than the generic one below.
  if (!is.null(.vtype)) {
    lifecycle::deprecate_stop(
      when = "0.2.0",
      what = "distribution(.vtype)",
      with = "distribution(.support)",
      details = c(
        i = paste(
          "A variable type cannot stand in for a support: it says what kind",
          "of probability there is, not where it lives."
        ),
        i = "Build one with `continuous()`, `discrete()`, or `mixed()`."
      )
    )
  }
  if (is.null(.support)) {
    stop(
      "A distribution needs a support.\n",
      "Pass `.support` a `continuous()`, `discrete()`, or `mixed()` set."
    )
  }
  support <- as_support(.support)
  if (is_empty_support(support)) {
    stop(
      "A distribution cannot have an empty support.\n",
      "It has to place its probability somewhere; see `?empty_support`."
    )
  }
  .vtype <- vtype_of_support(support)
  if (!is.null(.name)) {
    .name <- as.character(.name)
  } else {
    .name <- "Unnamed"
  }
  checkmate::assert_character(.name, len = 1, null.ok = TRUE)
  checkmate::assert_list(.parameters, names = "named", null.ok = TRUE)
  dots <- rlang::enquos(...)
  checkmate::assert_list(dots, names = "named", null.ok = TRUE)
  # `range` and `vtype` are properties of the distribution, but derived ones:
  # the support determines both. A stated entry would be consulted ahead of
  # the derived value and could disagree with it, so it is refused rather
  # than kept and ignored.
  derived <- intersect(c("range", "vtype"), names(dots))
  if (length(derived) > 0) {
    nm <- derived[[1L]]
    stop(
      "`", nm, "` is derived from the support, not stated.\n",
      "Drop it; `.support` determines it, and `", nm, "()` reads it."
    )
  }
  representations <- lapply(dots, rlang::eval_tidy)
  # Representations declaring variants only learn what they are a
  # representation *of* here, from the argument they were assigned to,
  # which is also when the levels they declare are checked.
  representations <- bind_representations(representations)
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
