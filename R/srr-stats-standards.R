#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} Distributions are generic enough to not need a specific
#' reference (e.g., an intro probability book will cover most concepts in
#' distionary), and it appears most other distribution packages on CRAN do
#' not include references.
#'
#' @srrstats {G1.1} The only relevant algorithm is the quantile algorithm.
#' It's included in the documentation that this algorithm is not new, and
#' was found on Stack Overflow somewhere, but unfortunately cannot find
#' the location anymore. --> Copied to `eval_quantile()`.
#'
#' @srrstats {G1.2} A Life Cycle Statement describing current and anticipated
#' future states of development can be found in the CONTRIBUTING file.
#'
#' @srrstats {G1.3} Users are referred to general texts in probability to learn
#' more about probability concepts and terminology (see README). Terms specific
#' to the package (like "distributional representation" are defined in
#' vignettes).
#'
#' @srrstats {G1.4} Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to
#' document all functions.
#' @srrstats {G1.4a} All internal (non-exported) functions are documented in
#' standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a
#' final `@noRd` tag to suppress automatic generation of `.Rd` files.
#'
#' ## ---- G 2 ----
#'
#' @srrstats {G2.0} Assertions on lengths of inputs (asserting that
#' inputs expected to be single- or multi-valued) are explicitly
#' tested for distribution parameters; implicitly through evaluation
#' functions.
#' @srrstats {G2.0a} Explicit secondary documentation of expectations on
#' lengths of inputs have been provided where relevant. See `dst_norm()`
#' for an example.
#'
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the
#' checkmate package for most functions.
#' @srrstats {G2.1a} Explicit secondary documentation of expectations on
#' data types of all vector inputs are provided. See `dst_norm()` for an
#' example.
#'
#' @srrstats {G2.2} Prohibiting or restricting submission of multivariate
#' input (i.e., distributions) to univariate parameters is
#' done using the checkmate package for relevant functions (e.g., `dst_*()`
#' specifications)
#'
#' @srrstats {G2.3} Univariate character input specifications are asserted
#' using the checkmate package where relevant (e.g., `.vtype` and `.name`
#' in `distribution()`; `arg_name` and `fn_prefix` in `enframe_*()`).
#' --> Copied to those functions.
#' @srrstats {G2.3a} `match.arg()` is used in the `plot()` method for
#' distributions, when specifying the representation to plot. --> Copied
#' to `plot()` function.
#' @srrstats {G2.3b} The use of `tolower()` is applicable for the `.name`
#' argument in `distribution()` and is used. --> Copied to `distribution()`.
#'
#' @srrstats {G2.4} Mechanisms to convert between different data types is
#' bypassed by requiring strict type inputs (except integer, which is allowed
#' to be integerish).
#' @srrstats {G2.4a} Explicit conversion to `integer` via `as.integer()`
#' becomes superfluous after checking that a number is "integerish" using the
#' checkmate package.
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()`
#' is avoided in case character input is provided; and error is thrown if the
#' input is not numeric, using the checkmate package.
#' @srrstats {G2.4c} Explicit conversion to character via `as.character()`
#' (and not `paste` or `paste0`) is done where character input is required:
#' `distribution()`'s `.vtype` and `.name` arguments, and the column naming
#' specifications of `enframe_general()`. --> Copied to both functions.
#'
#' @srrstats {G2.6} distionary asserts one-dimensional input where required
#' (e.g., `dst_*()` specifications) using the checkmate package.
#'
#' @srrstats {G2.13} Checks for missing data are conducted for distribution
#' parameters and a Null distribution is made to handle missing data.
#' See `dst_norm()` for an example. Checks are made for built-in
#' representations, but the onus is on the user for self-defined
#' distributions.
#'
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just
#' another data type. See `eval_*()` functions by way of example.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user
#' wants this behaviour, it should be explicitly specified in their code,
#' because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not
#' treated as special, but rather just another type of data, and therefore
#' does not need to alert the user of their presence.
#'
#' @srrstats {G2.15} Functions never assume non-missingness, and never
#' pass arguments to another function with `na.rm = FALSE`-type parameters.
#' This is most relevant for functions like `dst_norm()`.
#'
#' @srrstats {G2.16} This version of distionary does force the propagation of
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user
#' specification for length-stability, also because `Inf` and `-Inf` are
#' expected in some cases (e.g., the support of any Normal distribution).
#'
#' ## ---- G 3 ----
#'
#' @srrstats {G3.0} Appropriate tolerances for approximate equality is
#' adopted in instances of `expect_equal()`. The default is used, except
#' for instances where comparison can allow a larger tolerance. --> This
#' srrstats statement is included in all test files that use a different
#' tolerance in `expect_equal()` than the default.
#'
#' ## ---- G 5 ----
#'
#' @srrstats {G5.2} Appropriate error behaviour is tested for all
#' functions explicitly, but warnings are omitted and saved for a future
#' version. --> Copied to test-edge_cases-builtin_dists.R
#' @srrstats {G5.2a} While messages produced within R code by `stop()`,
#' `warning()`, `message()`, or equivalent are not unique (e.g.,
#' `dst_norm()`, `dst_pois()`, etc. all have the same `length != 1`
#' error message), they are unique enough to allow the user to debug.
#' @srrstats {G5.2b} Explicit tests trigger the `stop()` calls in this
#' version of distionary. --> Copied to test-edge_cases-builtin_dists.R
#'
#' @srrstats {G5.3} Functions that are expected to return objects containing no
#' missing (`NA`) or undefined (`NaN`, `Inf`) values are tested either
#' implicitly (e.g., `is_distribution()` implicitly checks non-NA value) or
#' explicitly (e.g., `pretty_name()` is never NA). --> Copied to both files.
#'
#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters. See `test-property-*.R` and `test-evaluation-*.R`.
#' @srrstats {G5.4b} Implementations of existing methods (cdf, density, ...)
#' are compared against the stats package where possible. Implementations
#' like the hazard function that are not found in the stats package are
#' implemented formulaically and verified by comparing to algorithm based
#' on the representation's definition.
#'
#' @srrstats {G5.5} Correctness tests are run with four fixed random seeds,
#' applicable for testing the `realise()` function (same as G5.6b). --> Copied
#' to `test-realise.R`. See, for example, `test-representation-chf.R` (copied
#' there).
#'
#' @srrstats {G5.6} Parameter recovery is relevant when distributional
#' properties (like quantiles) are computed from other properties (like the
#' cdf); these are possible to validate by comparing the intrinsic version
#' of the property against the derived version as if it were absent. See,
#' for example, `test-representation-density.R` (copied there).
#' @srrstats {G5.6a} Parameter recovery tests are conducted using the default
#' tolerance in the `testthat::expect_equal()` function whenever tests of
#' user-facing outputs are evaluated.
#' @srrstats {G5.6b} Parameter recovery tests are run with four fixed random
#' seeds, applicable for testing the `realise()` function. --> Copied to
#' `test-realise.R`.
#'
#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance. --> Copied to `test-representation-quantile.R`.
#'
#' @srrstats {G5.8} Edge conditions are tested when evaluating
#' representations. --> Copied to `test-edge_cases-*.R`.
#' @srrstats {G5.8a} Zero-length data input outputs 0-length vectors.
#' An error is thrown if zero-length parameters are input into `dst_*()`
#' functions.
#' @srrstats {G5.8b} Data of unsupported types does not pass the checkmate
#' checks on function inputs.
#' @srrstats {G5.8c} Data with all-`NA` fields or columns or all identical
#' fields or columns is no different from having some `NA` fields.
#' @srrstats {G5.8d} Data outside of the scope of the (quantile) algorithm
#' is only applicable when the quantile probability is outside of [0, 1],
#' in which case an error is thrown (due to a check for valid function
#' inputs)
#'
#' @srrstats {G5.9} Noise susceptibility tests have been conducted on
#' distribution parameters and evaluation inputs. --> Copied to
#' `test-machine_tolerance.R`.
#' @srrstats {G5.9a} Machine tolerance has been added to distribution
#' parameters and evaluation inputs and compared to originals. --> Copied to
#' `test-machine_tolerance.R`.
#'
#' ## ---- PD 1 ----
#'
#' @srrstats {PD1.0} Distributions are treated generally in distionary.
#'
#' ## ---- PD 2 ----
#'
#' @srrstats {PD2.0} Representing probability distributions using a package
#' for general representation is the main purpose of distionary.
#'
#' ## ---- PD 3 ----
#'
#' @srrstats {PD3.0} *Manipulation of probability distributions should
#' very generally be analytic, with numeric manipulations only
#' implemented with clear justification (ideally including references).*
#' This only applies to the Pearson Type III and Log Pearson Type III
#' that manipulates the Gamma distribution from the stats package analytically.
#' User-facing manipulation is the job of the `distplyr` package in the
#' probaverse family. --> Copied to `dst_lp3.R`.
#' @srrstats {PD3.1} Operations on probability distributions are
#' contained within separate functions which themselves accept the
#' names of the distributions as one input parameter. Examples include
#' the `eval_()` and `enframe_()` families of functions.
#' @srrstats {PD3.3} *Return objects which include values generated
#' from optimisation algorithms should include information on
#' optimisation algorithm and performance, minimally including the name
#' of the algorithm used, the convergence tolerance, and the number of
#' iterations.* This is specified for the only relevant algorithm, the
#' quantile algorithm. --> Copied to `eval_quantile.R`.
#' @srrstats {PD3.4} Noted that distribution integration is generally intended
#' for continuous distributions in this version of distionary, and does not
#' work so well with discrete components yet. --> Copied to
#' `eval_from_network-quantile.R`.
#'
#' ## ---- PD 4 ----
#'
#' @srrstats {PD4.0} The numeric outputs of probability distribution
#' functions are rigorously tested, not just output structures. These
#' tests are for numeric equality.
#'
#' @srrstats {PD4.1} Tests for numeric equality compare the output of
#' probability distribution functions with the output of code defined
#' in the same location in test files.
#'
#' @srrstats {PD4.2} All distributions are tested using at least two
#' valid parameter sets, and at least one invalid parameter set.
#'
#' @srrstats {PD4.3} Tests of optimisation or integration algorithms
#' compare derived results from built-in results for permutations of
#' every distribution parameter.
#'
#' @srrstats {PD4.4} Tests of optimisation or integration algorithms
#' compare derived results with algorithms in the stats package (e.g.,
#' quantile algorithm compared to `stats::q*()` functions).
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.5} No performance claims are made.
#' @srrstatsNA {G1.6} No performance claims are made.
#' @srrstatsNA {G2.4d} distionary does not work with factors, so explicit
#' conversion to factor via `as.factor()` is not needed.
#' @srrstatsNA {G2.4e} distionary does not work with factors, so explicit
#' conversion from factor via `as...()` is not needed.
#' @srrstatsNA {G2.5} No factors in distionary.
#' @srrstatsNA {G2.7} This software does not accept tabular input.
#' @srrstatsNA {G2.8} This software does not accept tabular input.
#' @srrstatsNA {G2.9} This software does not accept tabular input.
#' @srrstatsNA {G2.10} This software does not accept tabular input.
#' @srrstatsNA {G2.11} This software does not accept tabular input.
#' @srrstatsNA {G2.12} This software does not accept tabular input.
#' @srrstatsNA {G2.14c} Replacing data with imputed data does not make sense in
#' the context of distionary, so is not done.
#' @srrstatsNA {G3.1} This software does not rely on covariance calculations.
#' @srrstatsNA {G3.1a} This software does not rely on covariance calculations.
#' @srrstatsNA {G4.0} This software does not enable outputs to be written to
#' local files.
#' @srrstatsNA {G5.0} Data set inputs are not used by distionary.
#' @srrstatsNA {G5.1} Data set inputs are not used by distionary.
#' @srrstatsNA {G5.4a} No new methods are implemented in distionary.
#' @srrstatsNA {G5.4c} Stored values from published paper outputs (e.g.,
#' Normal quantile tables) are not relevant because they are accessible
#' via the stats package.
#' @srrstatsNA {G5.9b} Package does not rely on random draws, so different
#' seeds are not needed.
#' @srrstatsNA {G5.10} Extended tests are not needed in distionary because
#' all reasonable test permutations / combinations can be covered in a
#' reasonable amount of time and with available data.
#' @srrstatsNA {G5.11} Extended tests are not needed in distionary because
#' all reasonable test permutations / combinations can be covered in a
#' reasonable amount of time and with available data.
#' @srrstatsNA {G5.11a} Extended tests are not needed in distionary because
#' all reasonable test permutations / combinations can be covered in a
#' reasonable amount of time and with available data.
#' @srrstatsNA {G5.12} Extended tests are not needed in distionary because
#' all reasonable test permutations / combinations can be covered in a
#' reasonable amount of time and with available data.
#' @srrstatsNA {PD3.2} distionary does not estimate parameters (that will
#' be the job of the famish package in the probaverse family).
#' @srrstatsNA {PD3.5} Custom integration routines are not used in distionary;
#' the `stats::integrate()` routine is leveraged.
#' @srrstatsNA {PD3.5a} Custom integration routines are not used in distionary;
#' the `stats::integrate()` routine is leveraged.
#' @noRd
NULL
