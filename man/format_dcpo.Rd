% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_dcpo.R
\name{format_dcpo}
\alias{format_dcpo}
\title{Model estimation}
\usage{
format_dcpo(responses)
}
\arguments{
\item{responses}{a data frame with responses - typically the \code{responses}
element of a list returned by \link{generate_data}}
}
\value{
a list
}
\description{
Transforms data on responses to the format fitting DCPO (Solt's) model.
}
\details{
Adaptation of the original Solt's function of the same name included in the
DCPOtools package - compare https://github.com/fsolt/DCPOtools.
}
