% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling.R
\name{sample_chains}
\alias{sample_chains}
\title{Sample chains to produce valid parameter solution space}
\usage{
sample_chains(
  n_iter,
  r1,
  r2,
  xmin_pi = 0,
  xmax_pi = 1,
  xmin_2 = 0,
  xmax_2 = 1,
  alpha = FALSE,
  gamma = FALSE
)
}
\arguments{
\item{n_iter}{Number of iterations}

\item{r1}{Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance}

\item{r2}{Ratio of de novo cases verses detected cases under surveillance}

\item{xmin_pi}{Minimum value of uniform parameter scaling}

\item{xmax_pi}{Maximum value of uniform parameter scaling}

\item{xmin_2}{Minimum value of uniform parameter scaling}

\item{xmax_2}{Maximum value of uniform parameter scaling}

\item{alpha}{Flag for if want to sample alpha or phi}

\item{gamma}{Flag for if want to gamma alpha or pi}
}
\value{
List of parameter chains
}
\description{
Sample chains to produce valid parameter solution space
}
\examples{
sample_chains(10, 0.5, 3)
}
