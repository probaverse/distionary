% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dst_hyper.R
\name{dst_hyper}
\alias{dst_hyper}
\title{Hypergeometric Distribution}
\usage{
dst_hyper(m, n, k)
}
\arguments{
\item{m}{The number of red balls in the urn; single positive integer.}

\item{n}{The number of non-red balls in the urn; single positive integer.}

\item{k}{the number of balls drawn from the urn (between 0 and \code{m + n});
single positive integer.}
}
\value{
A Hypergeometric distribution.
}
\description{
Creates a Hypergeometric distribution, representing the
number of red balls drawn from an urn containing multiple colours,
using a scoop that holds a fixed number of balls.
}
\examples{
dst_hyper(15, 50, 10)
}
