% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_pmf.R
\name{eval_pmf}
\alias{eval_pmf}
\alias{enframe_pmf}
\title{Probability Mass Function}
\usage{
eval_pmf(distribution, at, strict = TRUE)

enframe_pmf(
  ...,
  at,
  arg_name = ".arg",
  fn_prefix = "pmf",
  sep = "_",
  strict = TRUE
)
}
\arguments{
\item{distribution, ...}{A distribution, or possibly multiple
distributions in the case of \code{...}.}

\item{at}{Vector of values to evaluate the cdf at. Must be named when using
in \code{enframe_}.}

\item{strict}{Only evaluate when the pmf exists? \code{TRUE} if so;
if \code{FALSE}, simply evaluates the probability of the specified outcomes.}

\item{arg_name}{For \code{enframe_}, name of the column containing
the function arguments.}

\item{fn_prefix}{For \code{enframe_}, name of the function to
appear in the column(s).}

\item{sep}{When \code{enframe}'ing more than one distribution, the
character that will be separating the \code{fn_name} and the distribution name.}
}
\value{
The evaluated probabilities
in vector form (for \code{eval_}) and data frame
or tibble form (for \code{enframe_}).
}
\description{
Access a distribution's probability mass function (pmf).
}
\examples{
d <- dst_empirical(1:10)
eval_pmf(d, at = c(1, 2, 2.5))
enframe_pmf(d, at = 0:4)
eval_pmf(dst_norm(0, 1), at = -3:3, strict = FALSE)
}
\seealso{
Other distributional representations: 
\code{\link{eval_cdf}()},
\code{\link{eval_chf}()},
\code{\link{eval_density}()},
\code{\link{eval_hazard}()},
\code{\link{eval_odds}()},
\code{\link{eval_quantile}()},
\code{\link{eval_return}()},
\code{\link{eval_survival}()}
}
\concept{distributional representations}
