#' @srrstatsVerbose TRUE
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
