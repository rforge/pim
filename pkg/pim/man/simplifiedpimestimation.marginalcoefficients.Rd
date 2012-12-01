\name{simplifiedpimestimation.marginalcoefficients}
\alias{simplifiedpimestimation.marginalcoefficients}
\alias{simplifiedpimestimation.marginalcovariance}
\alias{simplifiedpimestimation.pairwisecoefficients}
\alias{simplifiedpimestimation.pairwisecovariance}
\title{Simplified fitting of PIMs with only one categorical predictor}
\usage{
  simplifiedpimestimation.marginalcoefficients(data, out,
    group, block)

  simplifiedpimestimation.marginalcovariance(data, out,
    group, block)

  simplifiedpimestimation.pairwisecoefficients(data, out,
    group, block)

  simplifiedpimestimation.pairwisecovariance(data, out,
    group, block)
}
\arguments{
  \item{data}{Context where \code{out}, \code{group} and
  \code{block} are to be interpreted}

  \item{out}{Column of \code{data} that holds the outcomes
  (responses)}

  \item{group}{Column of \code{data} that holds the
  predicting variable}

  \item{block}{Column of \code{data} that holds the
  blocking variable. This can be left out if no blocking is
  present}
}
\value{
  for \code{simplifiedpimestimation.marginalcoefficients}:
  The coefficient estimates of the marginal model, in order
  of the levels of the predicting variable

  for \code{simplifiedpimestimation.marginalcovariance}:
  The covariance estimates of the marginal model, in order
  of the levels of the predicting variable

  for \code{simplifiedpimestimation.pairwisecoefficients}:
  The coefficient estimates of the pairwise model, in order
  of the combined levels of the predicting variable (as
  \code{combn} generates them)

  for \code{simplifiedpimestimation.pairwisecovariance}:
  The covariance estimates of the pairwise model, in order
  of the combined levels of the predicting variable (as
  \code{combn} generates them)
}
\description{
  Simplified fitting of PIMs with only one categorical
  predictor
}
\note{
  These functions are merely provided for comparison. The
  pim function supports generic ways of estimating these
  values.
}
\keyword{fit}
\keyword{pim}
\keyword{simplified}

