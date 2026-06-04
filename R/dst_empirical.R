#' Empirical Distribution
#'
#' An empirical distribution is a non-parametric way to
#' estimate a distribution using data. By default,
#' it assigns equal probability to all observations
#' (this can be overridden with the `weights` argument).
#' Identical to [dst_finite()] with NA handling and with weights not needing
#' to add to 1.
#'
#' @param y <`data-masking`> Numeric vector representing the potential
#' outcomes of the distribution.
#' @param weights <`data-masking`> Numeric vector of weights corresponding to
#' to the outcomes `y`. These will be scaled so that they add up to 1.
#' @param data Optionally, a data frame to compute `y` and `weights` from.
#' `NULL` if data are not coming from a data frame (the default).
#' @param na_action_y,na_action_w What should be done with `NA` entries in
#' `y` and `w`eights? Character vector of length 1: one of `"fail"`,
#' `"null"` (default), or `"drop"`. See details.
#' @returns A finite distribution. If only one outcome, returns a degenerate
#' distribution. Returns a Null distribution if `NA` values are present
#' and `"null"` is specified as an NA action.
#' @details
#' `y` and `weights` are recycled to have the same length, but only
#' if one of them has length 1 (via `vctrs::vec_recycle_common()`).
#'
#' `na_action_y` and `na_action_w` specify the NA action for `y` and `weights`.
#' Options are, in order of precedence:
#'
#' - `"fail"`: Throw an error in the presence of `NA`.
#' - `"null"`: Return a Null distribution (`dst_null()`) in the presence
#'   of `NA`.
#' - `"drop"` (the default for `na_action_w`): Remove outcome-weight pairs
#'   having an `NA` value in the specified vector.
#'
#' @seealso [dst_finite()]
#' @examples
#' t <- -2:7
#' dst_empirical(t)
#'
#' # Using a data frame
#' df <- data.frame(time = c(NA, NA, t))
#' dst_empirical(time * 60, data = df)  # Null, since `NA` in `time`.
#'
#' # Drop NA `time` values.
#' dst_empirical(time * 60, data = df, na_action_y = "drop")
#'
#' # Weights explicit. Zero-weight outcomes ("-120") are gone.
#' df$w <- c(1, 1, 0:9)
#' dst_empirical(time * 60, w, data = df, na_action_y = "drop")
#'
#' # "Null" takes precedence over "drop".
#' df$w <- c(NA, NA, 0:9)
#' df$time[1] <- -3
#' df$time[12] <- NA
#' dst_empirical(time, w, data = df, na_action_w = "null", na_action_y = "drop")
#' dst_empirical(time, w, data = df, na_action_w = "drop", na_action_y = "null")
#' dst_empirical(time, w, data = df, na_action_w = "drop", na_action_y = "drop")
#' @export
dst_empirical <- function(y,
                          weights = 1,
                          data = NULL,
                          na_action_y = c("null", "drop", "fail"),
                          na_action_w = c("null", "drop", "fail")) {
  enquo_y <- rlang::enquo(y)
  enquo_w <- rlang::enquo(weights)
  y <- rlang::eval_tidy(enquo_y, data = data)
  w <- rlang::eval_tidy(enquo_w, data = data)
  yw <- vctrs::vec_recycle_common(y, w)
  y <- yw[[1]]
  w <- yw[[2]]
  na_action_y <- rlang::arg_match(na_action_y)
  na_action_w <- rlang::arg_match(na_action_w)
  na_w <- is.na(w)
  na_y <- is.na(y)
  has_na_w <- any(na_w)
  has_na_y <- any(na_y)
  # Fail first
  if (has_na_w && na_action_w == "fail") {
    stop(
      "Weights have NA values. You can either deal with these, or choose ",
      "an alternate option for `na_action_w`."
    )
  }
  if (has_na_y && na_action_y == "fail") {
    stop(
      "Outcomes have NA values. You can either deal with these, or choose ",
      "an alternate option for `na_action_y`."
    )
  }
  # Null next
  if (has_na_w && na_action_w == "null") {
    return(dst_null())
  }
  if (has_na_y && na_action_y == "null") {
    return(dst_null())
  }
  # Drop last
  if (na_action_w == "drop") {
    y <- y[!na_w]
    w <- w[!na_w]
  }
  if (na_action_y == "drop") {
    na_y <- is.na(y) # Because y has a new length.
    y <- y[!na_y]
    w <- w[!na_y]
  }
  steps <- aggregate_weights(y, w, sum_to_one = TRUE)
  dst_finite(outcomes = steps$y, probs = steps$weight)
}
