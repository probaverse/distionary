#' @srrstatsVerbose TRUE
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
#' @srrstats {G5.3} Functions that are expected to return objects containing no
#' missing (`NA`) or undefined (`NaN`, `Inf`) values are tested either
#' implicitly (e.g., `is_distribution()` implicitly checks non-NA value) or
#' explicitly (e.g., `pretty_name()` is never NA).
#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters. See `test-property-*.R` and `test-evaluation-*.R`.
#' @srrstats {G5.4b} Implementations of existing methods (cdf, density, ...)
#' are compared against the stats package where possible. Implementations
#' like the hazard function that are not found in the stats package are
#' implemented formulaically and verified by comparing to algorithm based
#' on the representation's definition.
#' @srrstats {G5.5} Correctness tests are run with four fixed random seeds,
#' applicable for testing the `realise()` function (same as G5.6b). --> Copied
#' to `test-realise.R`. See, for example, `test-representation-chf.R` (copied
#' there).
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
#' inputs).
#'
#' @srrstats {PD4.0} The numeric outputs of probability distribution
#' functions are rigorously tested, not just output structures. These
#' tests are for numeric equality. --> Copied to test-pdq_gev.R.
#'
#' @srrstats {PD4.1} Tests for numeric equality compare the output of
#' probability distribution functions with the output of code defined
#' in the same location in test files. --> Copied to test-pdq_gev.R.
#'
#' @srrstats {PD4.2} All distributions are tested using at least two
#' valid parameter sets, and at least one invalid parameter set. --> Copied to test-parameters.R.
#'
#' @srrstats {PD4.3} Tests of optimisation or integration algorithms
#' compare derived results from built-in results for permutations of
#' every distribution parameter.
#'
#' @srrstats {PD4.4} Tests of optimisation or integration algorithms
#' compare derived results with algorithms in the stats package (e.g.,
#' quantile algorithm compared to `stats::q*()` functions).
#'
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
