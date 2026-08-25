#' Return Level Function
#'
#' Compute return levels (quantiles) from a distribution by inputting
#' return periods.
#'
#' @param at Vector of return periods >=1.
#' @inheritParams eval_cdf
#' @inherit eval_cdf return
#' @param event Which tail the event of interest lies in: `"upper"`, the
#' usual reading, makes the event an *exceedance* of the return level;
#' `"lower"` makes it a shortfall below it. A 100-year flood is an upper
#' event; a 100-year drought is a lower one.
#' @param obs_per_period How many observations of the variable make up one
#' unit of the return period. The default, 1, quotes return periods in
#' observations, which is what the distribution is a distribution of. Use 30
#' to quote return periods in months for a daily variable, 365 to quote them
#' in years, and so on.
#'
#' Read this as *an event occurring somewhere within a period of this many
#' observations*, which is the sense in which the probability rescales: the
#' event stays the same event, and only the window it is counted over
#' changes. It assumes those observations are independent of one another.
#'
#' It is emphatically **not** a conversion of the variable itself to a
#' coarser time scale. The monthly total of a daily variable, or its monthly
#' average, is a different quantity with a different distribution, and no
#' amount of rescaling probabilities will produce it. (The monthly *maximum*
#' is the exception: under independence, its return levels are exactly these
#' ones.)
#' @details This function is the quantile function evaluated at `1 - 1 / at`
#' for an upper event, and at `1 / at` for a lower one, after `at` is
#' converted to a number of observations.
#'
#' A distribution's own return function is taken to be the upper one, quoted
#' in observations. A distribution that can provide the lower one too can say
#' so with [variants()].
#' @examples
#' d <- dst_gp(24, 0.3)
#' eval_return(d, at = c(2, 25, 100, 200))
#'
#' # The same distribution, with return periods quoted in years
#' # for a variable observed daily.
#' eval_return(d, at = c(2, 25, 100), obs_per_period = 365)
#'
#' # Shortfalls rather than exceedances.
#' eval_return(dst_norm(0, 1), at = c(2, 25, 100), event = "lower")
#' @family distributional representations
#' @rdname return
#' @export
eval_return <- function(
  distribution,
  at,
  ...,
  event = c("upper", "lower"),
  obs_per_period = 1
) {
  rlang::check_dots_empty0(...)
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  event <- match_variant(event, "event", "return")
  checkmate::assert_number(obs_per_period, finite = TRUE)
  if (obs_per_period <= 0) {
    stop(
      "`obs_per_period` counts observations, so it must be positive.\n",
      "Received ", obs_per_period, "."
    )
  }
  # A return period is quoted in periods; the distribution knows only
  # observations. Converting here is exact, and leaves everything downstream
  # working in the one scale.
  eval_property(
    distribution,
    "return",
    at * obs_per_period,
    variant = variant(event = event, .entry = "return")
  )
}

#' @noRd
eval_return_from_network <- function(
  distribution,
  at,
  event = c("upper", "lower")
) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_numeric(at)
  event <- match_variant(event, "event", "return")
  if (event == "upper") {
    return(eval_quantile(distribution, at = 1 - 1 / at))
  }
  # Taken from the other end directly, rather than as one minus the upper
  # probability, which for a long return period would lose the very precision
  # the answer depends on.
  eval_quantile(distribution, at = 1 / at)
}

#' @rdname return
#' @export
enframe_return <- function(
  ...,
  at,
  event = "upper",
  obs_per_period = 1,
  arg_name = ".arg",
  fn_prefix = "return",
  sep = "_"
) {
  enframe_general(
    ...,
    at = at,
    arg_name = arg_name,
    fn_prefix = fn_prefix,
    sep = sep,
    eval_fn = eval_return,
    fn_args = list(event = event, obs_per_period = obs_per_period)
  )
}
