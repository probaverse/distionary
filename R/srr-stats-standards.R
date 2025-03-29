#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} Distributions are generic enough to not need a specific reference (e.g., an intro probability book will cover most concepts in distionary), and it appears most other distribution packages on CRAN do not include references.
#' @srrstats {G1.2} A Life Cycle Statement describing current and anticipated future states of development can be found in the CONTRIBUTING file.
#' @srrstats {G1.3} Users are referred to general texts in probability to learn more about probability concepts and terminology. Terms specific to the package (like "distributional representation" are defined in vignettes).
#' @srrstats {G1.4} Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.
#' @srrstats {G1.4a} All internal (non-exported) functions are documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.
#' @srrstats {G2.0} Assertions on lengths of inputs (asserting that
#' inputs expected to be single- or multi-valued) are explicitly
#' tested for distribution parameters; implicitly through evaluation
#' functions.
#' @srrstats {G2.1} Assertions on types of inputs is conducted using the checkmate package for most functions.
#' @srrstats {G2.2} Prohibiting or restricting submission of multivariate input (i.e., distributions) to parameters expected to be univariate is done using the checkmate package for relevant functions (e.g., `dst_*()` specifications)
#' @srrstats {G2.3} Univariate character input specifications are asserted using the checkmate package where relevant (e.g., `.vtype` and `.name` in `distribution()`; `arg_name` and `fn_prefix` in `enframe_*()`).
#' @srrstats {G2.3a} `match.arg()` is used in the `plot()` method for
#' distributions, when specifying the representation to plot.
#' @srrstats {G2.4} Mechanisms to convert between different data types is bypassed by requiring strict type inputs (except integer, which is allowed to be integerish).
#' @srrstats {G2.4a} Explicit conversion to `integer` via `as.integer()` becomes superfluous after checking that a number is "integerish" using the checkmate package.
#' @srrstats {G2.4b} Explicit conversion to continuous via `as.numeric()` is avoided in case character input is provided; and error is thrown if the input is not numeric, using the checkmate package.
#' @srrstats {G2.4c} Explicit conversion to character via `as.character()` (and not `paste` or `paste0`) is done where character input is required: `distribution()`'s `.vtype` and `.name` arguments, and the column naming specifications of `enframe_general()`.
#' @srrstats {G2.14} distionary is designed to propagate NA as if it's just another data type.
#' @srrstats {G2.14a} No option is given to error on missing data; if a user wants this behaviour, it should be explicitly specified in their code, because there is nothing fishy about NA inputs in the distionary context.
#' @srrstats {G2.14b} NA inputs are "ignored" in the sense that they are not treated as special, but rather just another type of data, and therefore does not need to alert the user of their presence.
#' @srrstats {G2.14c} Replacing data with imputed data does not make sense in the context of distionary, so is not done.
#' @srrstats {G2.16} This version of distionary does forces the propagation of undefined values (e.g., `NaN`, `Inf` and `-Inf`) rather than allowing user specification for length-stability, also because `Inf` and `-Inf` are expected in some cases (e.g., the support of any Normal distribution).

#' @srrstats {G5.6} Parameter recovery is relevant when distributional properties (like quantiles) are computed from other properties (like the cdf); these are all tested in `test-distributions_valid.R` when the computational version is compared to the embedded property.
#' @srrstats {G5.6a} Parameter recovery tests are conducted using a reasonable tolerance.
#' @srrstats {G2.6} distionary asserts one-dimensional input where required (e.g., `dst_*()` specifications) using the checkmate package.
#' @srrstats {G3.0} Appropriate tolerances for approximate equality is
#' adopted (stricter tolerances planned for future based on discretes
#' tracking design). See `test-distributions_valid.R`.
#' @srrstats {G5.3} Functions that are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values are tested either implicitly (e.g., `is_distribution()` implicitly checks non-NA value) or explicitly (e.g., `pretty_name()` is never NA).
#' @srrstats {G5.2} Appropriate error behaviour is tested for all
#' functions explicitly, but warnings are omitted and saved for a future
#' version. See `test-distributions_valid.R`.
#' @srrstats {G5.2b} Explicit tests trigger the `stop()` calls in this
#' version of distionary. See `test-distributions_valid.R`.
#' @srrstats {G5.4} Correctness tests are conducted to test that
#' statistical algorithms (calculating properties from other distribution
#' properties) produce expected results to test distributions with set
#' parameters. See `test-distributions_valid.R`.
#' @srrstats {G5.4b} New implementations of existing methods are compared
#' against the stats package where possible. Implementations like the
#' hazard function that are not found in the stats package are compared
#' to known formulas rather than other implementations, to avoid unnecessary
#' dependencies on other packages. See `test-distributions_valid.R`.
#' @srrstats {G5.7} The only algorithm thus far is the quantile algorithm,
#' and its performance has been tested to take longer with a smaller
#' tolerance.
#' @srrstats {PD1.0} Distributions are treated generally in distionary.
#' @srrstats {PD2.0} Representing probability distributions using a package
#' for general representation is the main purpose of distionary.
#' @srrstats {PD4.0} The numeric outputs of probability distribution
#' functions are rigorously tested, not just output structures. These
#' tests are for numeric equality. See `test-distributions_valid.R`.
#' @srrstats {PD4.1} Tests for numeric equality compare the output of
#' probability distribution functions with the output of code defined
#' in the same location in test files. See `test-distributions_valid.R`.
#' @srrstats {PD4.2} All distributions are tested using at least two
#' valid parameter sets, and at least one invalid parameter set.
#' See `test-distributions_valid.R`.
#' @srrstats {PD4.3} Tests of optimisation or integration algorithms
#' compare derived results from built-in results for permutations of
#' every distribution parameter. See `test-distributions_valid.R`.
#' @srrstats {PD4.4} Tests of optimisation or integration algorithms
#' compare derived results with algorithms in the stats package.
#' See `test-distributions_valid.R`.
#'
#' TO DO
#' @srrstats {PD3.5} Integration routines should only rely on discrete summation where such use can be justified (for example, through providing a literature reference), in which case the following applies:
#' @srrstats {PD3.5a} Use of discrete summation to approximate integrals must demonstrate that the Reimann sum has a finite limit (or, equivalently, must explicitly describe the conditions under which the sum may be expected to be finite).



#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G1.5} No performance claims.
#' @srrstatsNA {G1.6} No performance claims.
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
#' @srrstatsNA {G5.5} No tests depend on random number generation. (There is
#' one for `dst_degenerate()`, but this should always generate the same
#' value regardless of the seed).
#' @srrstatsNA {G5.6b} Parameter recovery tests do not need random number
#' generation and therefore do not need to use seeds.
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
#' @noRd
NULL
