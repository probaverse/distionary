% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.dst}
\alias{plot.dst}
\title{Plot a Distribution}
\usage{
\method{plot}{dst}(
  x,
  what = c("density", "cdf", "survival", "quantile", "hazard", "chf"),
  ...
)
}
\arguments{
\item{x}{Distribution object}

\item{what}{Name of the representation to plot.}

\item{...}{Other arguments to pass to the \code{graphics::curve} function.
plot(dst_norm(5, 5))
plot(dst_empirical(-5:2), "cdf", n = 1001)}
}
\description{
Plot a functional representation of a distribution.
Wrapper around the \code{graphics::curve} function.
}
