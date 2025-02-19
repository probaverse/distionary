#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} A Life Cycle Statement describing current and anticipated future states of development can be found in the CONTRIBUTING file.
#' @srrstats {G1.4} Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.
#' @srrstats {G1.4a} All internal (non-exported) functions are documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.
#' @srrstats {PD2.0} Representing probability distributions using a package for general representation is the main purpose of distionary.
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G1.0} Distributions are generic enough to not need a specific reference (e.g., an intro probability book will cover most concepts in distionary).
#' @srrstatsNA {G1.3} Users are referred to general texts in probability to learn more about probability concepts and terminology. Terms specific to the package (like "distributional representation" are defined in vignettes).
#' @srrstatsNA {G1.5} No performance claims.
#' @srrstatsNA {G1.6} No performance claims.
#' @srrstatsNA {G2.1} Assertions on types of inputs is not typically done in this version of distionary; right now the onus is on the user.
#' @srrstatsNA {G2.2} Prohibiting or restricting submission of multivariate input (i.e., distributions) to parameters expected to be univariate is not done in this version, and the onus is on the user right now.
#' @srrstatsNA {G2.3} Univariate character input specifications are not relevant for distionary.
#' @srrstatsNA {G2.3a} `match.arg()` or equivalent is not applicable, since specific character inputs are not required.
#' @srrstatsNA {G2.3b} The use of `tolower()` or equivalent is not used because for this version of distionary the onus is on the user.
#' @srrstatsNA {G2.4} Appropriate mechanisms to convert between different data types is not applied in this version and the onus is on the user for now.
#' @srrstatsNA {G2.4a} Explicit conversion to `integer` via `as.integer()` is not conducted because the onus is on the user for this version.
#' @srrstatsNA {G2.4b} Explicit conversion to continuous via `as.numeric()` is not conducted because the onus is on the user for this version.
#' @srrstatsNA {G2.4c} Explicit conversion to character via `as.character()` (and not `paste` or `paste0`) is not conducted because the onus is on the user for this version.
#' @srrstatsNA {G2.4d} Explicit conversion to factor via `as.factor()` is not needed because distionary does not work with factors.
#' @srrstatsNA {G2.4e} Explicit conversion from factor via `as...()` functions is not needed because distionary does not work with factors.
#' @srrstatsNA {G2.5} No factors in distionary.
#' @srrstatsNA {G2.6} distionary accepts one-dimensional input (i.e., a single distribution) but does not ensure values are pre-processed; for this version, the onus is on the user.
#' @srrstatsNA {G2.7} This software does not accept tabular input.
#' @srrstatsNA {G2.8} This software does not accept tabular input.
#' @srrstatsNA {G2.9} This software does not accept tabular input.
#' @srrstatsNA {G2.10} This software does not accept tabular input.
#' @srrstatsNA {G2.11} This software does not accept tabular input.
#' @srrstatsNA {G2.12} This software does not accept tabular input.
#' @srrstatsNA {G2.14} This version forces either the creation of a Null distribution or evaluates NA input to NA output.
#' @srrstatsNA {G2.14a} This version does not give the option to error on NA inputs.
#' @srrstatsNA {G2.14b} This version does not give the option to ignore missing data with default warnings or messages issued.
#' @srrstatsNA {G2.14c} This version does not give the option to replace missing data with appropriately imputed values.
#' @srrstatsNA {G2.16} This version of distionary does not provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.
#' @srrstatsNA {G3.1} Does not rely on covariance calculations.
#' @srrstatsNA {G3.1a} Does not rely on covariance calculations.
#' @srrstatsNA {G4.0} Does not enable outputs to be written to local files.
#' @srrstatsNA {G5.0} Data set inputs are not used by distionary.
#' @srrstatsNA {G5.1} Data set inputs are not used by distionary.
#' @srrstatsNA {G5.3} No functions are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values.
#' @srrstatsNA {G5.4a} No new methods are implemented.
#' @srrstatsNA {G5.4c} Stored values from published paper outputs (e.g., Normal quantile tables) are not relevant because they are accessible via the stats package.
#' @srrstatsNA {G5.5} No tests depend on random number generation. (There is one for `dst_degenerate()`, but this should always generate the same value regardless of the seed).
#' @srrstatsNA {G5.6} Parameter recovery tests are not valid because parameters are never estimated.
#' @srrstatsNA {G5.6a} Parameter recovery tests are not valid because parameters are never estimated.
#' @srrstatsNA {G5.6b} Parameter recovery tests are not valid because parameters are never estimated.
#' @srrstatsNA {G5.7} Algorithm performance tests are not applicable, performance is not relevant for this version of distionary.
#' @srrstatsNA {G5.8} Edge condition tests are not always handled in this version, and the onus is on the user.
#' @srrstatsNA {G5.8a} Zero-length data: the onus is on the user.
#' @srrstatsNA {G5.8b} Data of unsupported types: the onus is on the user.
#' @srrstatsNA {G5.8c} Data with all-`NA` fields or columns or all identical fields or columns: the onus is on the user.
#' @srrstatsNA {G5.8d} Data outside the scope of the algorithm: the onus is on the user.
#' @srrstatsNA {G5.9} Noise susceptibility tests are not conducted for this version of distionary.
#' @srrstatsNA {G5.9a} Noise susceptibility tests are not conducted for this version of distionary.
#' @srrstatsNA {G5.9b} Noise susceptibility tests are not conducted for this version of distionary.
#' @srrstatsNA {G5.10} Extended tests are not considered in this version of distionary.
#' @srrstatsNA {G5.11} Extended tests are not considered in this version of distionary.
#' @srrstatsNA {G5.11a} No downloads of data are necessary for distionary to function.
#' @srrstatsNA {G5.12} Extended tests are not considered in this version of distionary.
#' @srrstatsNA {PD1.0} Distributions are treated generally in distionary.
#' @srrstatsNA {PD3.2} distionary does not estimate parameters (that will be the job of the famish package in the probaverse family).
#' @srrstatsNA {PD3.5a} Discrete summation is not used to approximate integrals.
#' @noRd
NULL
