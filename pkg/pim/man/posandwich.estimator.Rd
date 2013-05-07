\name{posandwich.estimator}
\alias{posandwich.estimator}
\alias{posandwich.estimator.Uforposandwich}
\alias{Uforposandwich}
\alias{Uforposandwich-class}
\alias{Uforposandwich.default}
\alias{Uforposandwich.fakeH0}
\alias{variance.estimator}
\title{Pseudo-observation variance sandwich estimator}
\usage{
  posandwich.estimator(U, U.diff, g1, g2,
    shared.factor = 1, switched.factor = 1,
    self.factor = 1, verbosity = 0)

  posandwich.estimator.Uforposandwich(Uforposandwich,
    poset, verbosity = 0)

  Uforposandwich.default(Zbeta, Z, Y, link, W = NULL)

  Uforposandwich.fakeH0(Zbeta, Z, Y, link, W = NULL)
}
\arguments{
  \item{U}{See the formula for sandwich estimator: holds
  \code{U_{ij}}}

  \item{U.diff}{See the formula for sandwich estimator:
  holds the partial derivatives of \code{U}.}

  \item{g1,g2}{Index in the original observations of the
  "left" and "right" part of the pseudo- observations.}

  \item{shared.factor}{Factor by which all \code{UijUik} or
  \code{UijUlj} will be multiplied}

  \item{switched.factor}{Factor by which all \code{UijUki}
  or \code{UijUjl} will be multiplied}

  \item{self.factor}{Factor by which all \code{UijUij} or
  \code{-UijUji} will be multiplied}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{Uforposandwich}{Result of a function of the form of
  \code{Uforposandwich.default} (note this should be of
  class \code{"Uforposandwich"})}

  \item{poset}{Matrix of two columns indicating what the
  original observation number is for the left and right
  real observation in the pseudo-observation.}

  \item{Zbeta}{Matrix holding \code{Z^T beta}}

  \item{Z}{Design Matrix in the pseudo-observations.}

  \item{Y}{Pseudo-outcomes.}

  \item{link}{Name of the link function.}

  \item{W}{Weights to be applied to the
  pseudo-observations.}
}
\value{
  For \code{posandwich.estimator(.Uforposandwich)}: the
  estimated covariance matrix.

  For
  \code{Uforposandwich.default}/\code{Uforposandwich.fakeH0}:
  an object of class \code{"Uforposandwich"}, holding
  items: \item{U}{see the matching parameter to
  \code{posandwich.estimator}} \item{U.diff}{see the
  matching parameter to \code{posandwich.estimator}}
  \item{fv}{Inversely transformed \code{Zbeta}. The
  predicted probabilistic indicator for each
  pseudo-observation.} \item{shared.factor}{see the
  matching parameter to \code{posandwich.estimator}}
  \item{switched.factor}{see the matching parameter to
  \code{posandwich.estimator}} \item{self.factor}{see the
  matching parameter to \code{posandwich.estimator}}
}
\description{
  Pseudo-observation variance sandwich estimator
}
\details{
  \code{\link{posandwich.estimator.Uforposandwich}} is
  essentially a wrapper that translates parameters.

  \code{\link{Uforposandwich.default}} is the normal
  function for the sandwich estimator.
  \code{\link{Uforposandwich.fakeH0}} is provided to
  generally estimate the variance under H0 (with identity
  link) through the same sandwich estimator code.
}
\note{
  There should be no reason to use
  \code{posandwich.estimator} outside of its usage in
  \code{\link{varianceestimator.sandwich}}...
}
\seealso{
  \code{\link{varianceestimator.sandwich}}
}
\keyword{covariance}
\keyword{pim}
\keyword{sandwich}

