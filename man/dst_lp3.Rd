% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dst_lp3.R
\name{dst_lp3}
\alias{dst_lp3}
\title{Log Pearson Type III distribution}
\usage{
dst_lp3(meanlog, sdlog, skew)
}
\arguments{
\item{meanlog}{Mean of the log of the random variable; single numeric.}

\item{sdlog}{Standard deviation of the log of the random variable;
single positive numeric.}

\item{skew}{Skewness of the log of the random variable;
single numeric.}
}
\value{
A Log Pearson Type III distribution.
}
\description{
Makes a Log Pearson Type III distribution, which is the
distribution of the exponential of a random variable following
a Pearson Type III distribution.
}
\examples{
dst_lp3(0, 1, 1)
}
