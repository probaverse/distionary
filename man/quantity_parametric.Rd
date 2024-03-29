% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantities-parametric.R
\name{quantity_parametric}
\alias{quantity_parametric}
\title{Grab a quantity from the database}
\usage{
quantity_parametric(distribution, quantity)
}
\arguments{
\item{distribution}{A distribution.}

\item{quantity}{Character; name of the quantity to extract.}
}
\value{
The desired quantity, evaluated.
}
\description{
For a parametric distribution, evaluates a quantity
(such as mean, median, range, ...) if the distribution has
an entry in the \code{.quantities} database. For distributions without
the "parametric" (sub-) class, quantity evaluation is delegated
to the next higher level class.
}
