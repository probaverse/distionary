#' @export
refine_parameters <- function(distribution, ...) {
  pairs <- rlang::enquos(...)
  pairs <- lapply(pairs, rlang::eval_tidy)
  distribution$params <- append(distribution$params, pairs)
  distribution
}


#' @export
param_resolve <- function(distribution, ...) {
  ell <- rlang::enquos(...)
  names_ <- names(ell)
  # childenv <- rlang::new_environment(parent = distribution$bottom)
  # distribution$bottom <- childenv
  # mask <- rlang::new_data_mask(distribution$bottom, distribution$top)
  for (i in seq_along(ell)) {
    name_ <- names_[i]
    rlang::eval_tidy(rlang::expr(assign(name_, ell[[i]])), data = distribution$mask)
  }
  check_params(ell)
  distribution
}

#' @export
restrict_params <- function(distribution, ..., .env) {
  new_restrictions <- rlang::enquos(...)
  distribution$paramspace <- append(distribution$paramspace, new_restrictions)
}
