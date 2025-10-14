#' @srrstats {G1.2} A Life Cycle Statement describing current and anticipated
#' future states of development can be found in the CONTRIBUTING file.
#' @srrstats {G1.3} Users are referred to general texts in probability to learn
#' more about probability concepts and terminology (see README). Terms specific
#' to the package (like "distributional representation" are defined in
#' vignettes).
#'
#' @section Overview:
#' The \pkg{distionary} package provides a comprehensive framework for working
#' with probability distributions in R. With \pkg{distionary}, you can:
#' \enumerate{
#'   \item Specify probability distributions from common families or create
#'         custom distributions.
#'   \item Evaluate distributional properties and representations.
#'   \item Access distributional calculations even when they're not directly
#'         specified.
#' }
#'
#' The main purpose of \pkg{distionary} is to implement a distribution object
#' that powers the wider \href{https://probaverse.com}{probaverse ecosystem} for
#' making probability distributions that are representative of your data.
#'
#' @section Creating Distributions:
#' Use the \code{dst_*()} family of functions to create distributions from
#' common families:
#' \itemize{
#'   \item \code{\link{dst_norm}()}, \code{\link{dst_exp}()},
#'         \code{\link{dst_unif}()}, etc. are some continuous distributions.
#'   \item \code{\link{dst_pois}()}, \code{\link{dst_binom}()},
#'         \code{\link{dst_geom}()}, etc. are some discrete distributions.
#'   \item \code{\link{dst_empirical}()} is useful for creating a non-parametric
#'         distribution from data.
#' }
#' You can also make your own distribution using the
#' \code{\link{distribution}()} function, which allows you to specify
#' any combination of distributional representations and properties. For this
#' version of \pkg{distionary}, the CDF and density/PMF are required
#' in order to access all functionality.
#'
#' @section Evaluating Distributions:
#' A distribution's _representations_ are functions that fully describe the
#' distribution. They can be accessed with the \code{eval_*()} functions.
#' For example, \code{\link{eval_cdf}()} and \code{\link{eval_quantile}()}
#' invoke the distribution's cumulative distribution function (CDF)
#' and quantile function.
#'
#' Other properties of the distribution can be calculated by functions of the
#' property's name, such as \code{\link{mean}()} and \code{\link{range}()}.
#'
#' @section Random Samples:
#' Generate random samples from a distribution using
#' \code{\link{realise}()}.
#'
#' @section Getting Started:
#' New users should start with the package vignettes:
#' \itemize{
#'   \item \code{vignette("specify", package = "distionary")} -
#'         Learn how to specify distributions.
#'   \item \code{vignette("evaluate", package = "distionary")} -
#'         Learn how to evaluate distributions.
#' }
#'
#' @examples
#' # Create a Poisson distribution.
#' poisson <- dst_pois(lambda = 1.5)
#' poisson
#'
#' # Evaluate the probability mass function.
#' eval_pmf(poisson, at = 0:4)
#' plot(poisson)
#'
#' # Get distribution properties.
#' mean(poisson)
#' variance(poisson)
#'
#' # Create a continuous distribution (Normal).
#' normal <- dst_norm(mean = 0, sd = 1)
#'
#' # Evaluate quantiles.
#' eval_quantile(normal, at = c(0.025, 0.5, 0.975))
#'
#' # Create a custom distribution.
#' my_dist <- distribution(
#'   density = function(x) ifelse(x >= 0 & x <= 1, 2 * (1 - x), 0),
#'   cdf = function(x) ifelse(x >= 0 & x <= 1, 1 - (1 - x)^2, 0),
#'   .vtype = "continuous",
#'   .name = "Linear"
#' )
#' plot(my_dist)
#' plot(my_dist, "cdf")
#'
#' # Even without specifying all properties, they can still be computed.
#' mean(my_dist)
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
