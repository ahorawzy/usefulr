% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_operator.R
\name{delay_by}
\alias{delay_by}
\title{FO: delay_by}
\usage{
delay_by(delay, f)
}
\arguments{
\item{delay}{The delay seconds.}

\item{f}{The base function.}
}
\description{
This FO can make function sleep n seconds before operating.
}
\examples{
system.time(delay_by(1,runif)(100))

}
