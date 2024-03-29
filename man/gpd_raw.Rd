% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/representations-dst_gpd.R
\name{pgpd}
\alias{pgpd}
\alias{qgpd}
\alias{dgpd}
\title{Representations of the Generalized Pareto Distribution}
\usage{
pgpd(q, location, scale, shape, lower.tail = TRUE)

qgpd(p, location, scale, shape)

dgpd(x, location, scale, shape)
}
\arguments{
\item{location}{Location parameter; numeric.}

\item{scale}{Scale parameter; positive numeric.}

\item{shape}{Shape parameter; numeric.}

\item{lower.tail}{Logical. If \code{TRUE}, cdf (default);
if \code{FALSE}, survival function.}

\item{p}{Vector of probabilities.}

\item{x, q}{Vector of quantiles.}
}
\description{
Representations of the Generalized Pareto Distribution
}
\examples{
pgpd(1:10, 0, 1, 1)
dgpd(1:10, 0, 2, 0)
qgpd(1:9 / 10, 2, 10, -2)
}
