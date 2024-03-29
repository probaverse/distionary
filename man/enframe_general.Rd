% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enframe.R
\name{enframe_general}
\alias{enframe_general}
\title{Enframe a distributional representation's evaluation}
\usage{
enframe_general(..., at, arg_name, fn_prefix, sep, eval_fn, fn_args = list())
}
\arguments{
\item{at}{Vector of values to evaluate the cdf at. Must be named when using
in \code{enframe_}.}

\item{arg_name}{For \code{enframe_}, name of the column containing
the function arguments.}

\item{fn_prefix}{For \code{enframe_}, name of the function to
appear in the column(s).}

\item{sep}{When \code{enframe}'ing more than one distribution, the
character that will be separating the \code{fn_name} and the distribution name.}

\item{eval_fn}{Name of the \code{eval_} function for the desired distributional
representation, such as \code{eval_cdf} and \code{eval_density}.}

\item{fn_args}{A named list of arguments to pass to the \code{eval_fn} function,
besides the distribution and \code{at} argument (the \code{strict} argument
being the most common, and perhaps the only use case).}
}
\value{
A data frame or tibble of the input argument (\code{at}), with the
evaluated distributional representation for each distribution in
\code{...} in its own column.
}
\description{
This is the workhorse for the \code{enframe_} family of functions.
\code{enframe_general()} evaluates a specified distributional representation
for multiple distributions, and places the results in a data frame
or tibble.
}
\details{
If only one distribution is specified in \code{...}, then the evaluation
column will be named that of \code{fn_prefix}.

If more than one distribution
is specified in \code{...}, the evaluation columns will be named by the
prefix \code{fn_prefix} followed by the distribution names, with \code{sep} in between.

Distributions are named first by their argument names, if given, or if not,
the input text. Names are then made unique using \code{vctrs::vec_as_names()}
with the "unique" names repair. "Unique" is chosen instead of "universal"
because names are anticipated to be syntactic with the \code{eval_fn} prefix;
"minimal" is not sufficient because it may result in columns having the
same names.
}
