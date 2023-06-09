% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_dcpo.R
\name{estimate_dcpo}
\alias{estimate_dcpo}
\title{Model estimation}
\usage{
estimate_dcpo(
  responses,
  model,
  iter = 1000L,
  pars = list(max_treedepth = 14, adapt_delta = 0.99, step_size = 0.005, seed = 3724,
    chains = 4, parallel_chains = 4, iter_warmup = iter/2, iter_sampling = iter/2,
    refresh = iter/10, init = 0.1)
)
}
\arguments{
\item{responses}{a data frame with responses - typically the \code{responses}
element of a list returned by \link{generate_data}}

\item{model}{an object of class \emph{CmdStanModel} representing Solt's model}

\item{iter}{a positive integer - number of MCMC iterations}

\item{pars}{a list of additional parameters that are passed to the \code{model}'s
\code{sample} method}
}
\value{
a list with two elements:
\describe{
\item{modelSummary}{a data frame with model estimation time and \emph{rhat}}
\item{countryMeans}{a data frame with country-year means estimates}
}
}
\description{
Estimates Solt's DCPO model.
}
