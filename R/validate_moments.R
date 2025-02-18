#' @family validate_properties
#' @noRd
validate_mean <- function(
    distribution, verbose = FALSE, tol = 1e-05) {
  if (is.null(distribution$mean)) {
    return(NA)
  }
  mean_builtin <- distribution$mean
  distribution$mean <- NULL
  mean_derived <- mean(distribution)
  if (identical(mean_builtin, mean_derived) ||
    (is.infinite(mean_builtin) && is.nan(mean_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(mean_builtin - mean_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated mean differs from stored mean by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}

validate_variance <- function(
    distribution, verbose = FALSE, tol = 1e-05) {
  if (is.null(distribution$variance)) {
    return(NA)
  }
  variance_builtin <- distribution$variance
  distribution$variance <- NULL
  variance_derived <- variance(distribution)
  if (identical(variance_builtin, variance_derived) ||
    (is.infinite(variance_builtin) && is.nan(variance_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(variance_builtin - variance_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated variance differs from stored variance by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}

validate_stdev <- function(
    distribution, verbose = FALSE, tol = 1e-05) {
  if (is.null(distribution$stdev)) {
    return(NA)
  }
  stdev_builtin <- distribution$stdev
  distribution$stdev <- NULL
  stdev_derived <- stdev(distribution)
  if (identical(stdev_builtin, stdev_derived) ||
    (is.infinite(stdev_builtin) && is.nan(stdev_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(stdev_builtin - stdev_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated stdev differs from stored stdev by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}

validate_skewness <- function(
    distribution, verbose = FALSE, tol = 1e-04) {
  if (is.null(distribution$skewness)) {
    return(NA)
  }
  skewness_builtin <- distribution$skewness
  distribution$skewness <- NULL
  skewness_derived <- skewness(distribution)
  if (identical(skewness_builtin, skewness_derived) ||
    (is.infinite(skewness_builtin) && is.nan(skewness_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(skewness_builtin - skewness_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated skewness differs from stored skewness by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}

validate_kurtosis <- function(
    distribution, verbose = FALSE, tol = 1e-04) {
  if (is.null(distribution[["kurtosis"]])) {
    return(NA)
  }
  kurtosis_builtin <- distribution[["kurtosis"]]
  distribution[["kurtosis"]] <- NULL
  kurtosis_derived <- kurtosis(distribution)
  if (identical(kurtosis_builtin, kurtosis_derived) ||
    (is.infinite(kurtosis_builtin) && is.nan(kurtosis_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(kurtosis_builtin - kurtosis_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated kurtosis differs from stored kurtosis by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}

validate_kurtosis_exc <- function(
    distribution, verbose = FALSE, tol = 1e-04) {
  if (is.null(distribution[["kurtosis_exc"]])) {
    return(NA)
  }
  kurtexc_builtin <- distribution[["kurtosis_exc"]]
  distribution[["kurtosis_exc"]] <- NULL
  kurtexc_derived <- kurtosis_exc(distribution)
  if (identical(kurtexc_builtin, kurtexc_derived) ||
    (is.infinite(kurtexc_builtin) && is.nan(kurtexc_derived))) {
    diff_ <- 0
  } else {
    diff_ <- abs(kurtexc_builtin - kurtexc_derived)
  }
  if (diff_ < tol) {
    return(TRUE)
  } else {
    if (verbose) {
      message(
        "Calculated kurtexc differs from stored kurtexc by ",
        signif(diff_, 4), "."
      )
    }
    return(FALSE)
  }
}
