\name{varianceestimator.sandwich}
\alias{varianceestimator.H0}
\alias{varianceestimator.bootstrap}
\alias{varianceestimator.glm}
\alias{varianceestimator.sandwich}
\title{Estimators for the PIM parameter variances}
\usage{
  varianceestimator.sandwich(Uforposandwich = Uforposandwich.default)

  varianceestimator.H0(Uforposandwich = Uforposandwich.fakeH0)

  varianceestimator.glm(control = list())

  varianceestimator.bootstrap(D = 500,
    keep.posetbs = FALSE)
}
\arguments{
  \item{Uforposandwich}{function like
  \code{\link{Uforposandwich.default}} (the default) to
  calculate the necessary parts for
  \code{\link{posandwich.estimator}}.}

  \item{control}{See \code{\link{glm.fit}}.}

  \item{D}{Number of bootstrap repetitions (defaults to
  500).}

  \item{keep.posetbs}{If this is \code{TRUE} (not the
  default), the \code{morevarfitinfo} will be a list of two
  items: \code{allcoefs} containing the fitted
  coefficients, and \code{posetbs}, containing which
  pseudoobservations were bootstrapped. In both, the
  bootstrap repetitions are in the columns.}
}
\value{
  These functions (\code{varianceestimator.*}) each return
  a function themselves. The returned function should have
  five parameters (\code{estimationresult}, the parameter
  estimates themselves (result of \code{estimation.*});
  \code{pfd}, an object of class \code{\link{pimfitdata}} ;
  \code{link}, the name of the link function;
  \code{estimator}, the estimator used to generate
  \code{estimationresult}; \code{verbosity}, see elsewhere)
  and should itself return a list of two items:
  \item{vcov}{The estimated covariance matrix}
  \item{morevarfitinfo }{Implementation specific
  information on the variance estimation}
}
\description{
  Estimators for the PIM parameter variances
}
\details{
  These functions estimate the variances of the parameter
  estimates of the PIM.  Each provides a solution under
  different circumstances and some may be more accurate /
  correct / performant depending on the specific model.

  For the different implementations, \code{morevarfitinfo}
  contains: \enumerate{ \item
  \code{varianceestimator.sandwich}The return value of the
  \code{\link{Uforposandwich}} call. This estimate is the
  generally valid sandwich estimator for the (co)variance.
  \item \code{varianceestimator.H0}The return value of the
  \code{\link{Uforposandwich}} call. This estimate is the
  estimator for the (co)variance under H0. \item
  \code{varianceestimator.glm}The return value of the
  \code{\link{glm}} call. This estimate is only valid if
  the pseudo-observations are somehow independent and the
  linkfunction is identity. \item
  \code{varianceestimator.bootstrap}A matrix holding the
  coefficient estimates over all bootstrap repetitions. The
  columns represent the repetitions }
}
\keyword{covariance}
\keyword{estimate}
\keyword{pim}

