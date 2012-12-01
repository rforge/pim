\name{estimator.nleqslv}
\alias{estimator.BB}
\alias{estimator.glm}
\alias{estimator.glmnet}
\alias{estimator.nleqslv}
\alias{estimator.trymultiple}
\alias{scorefunctioncreator.default}
\title{Estimators for the PIM parameters}
\usage{
  estimator.nleqslv(jac = NULL,
    method = c("Broyden", "Newton"),
    global = c("dbldog", "pwldog", "qline", "gline", "none"),
    xscalm = c("fixed", "auto"), control = list(),
    scoreFunctionCreator = scorefunctioncreator.default,
    ignore.error = FALSE)

  estimator.glm(control = list())

  estimator.glmnet(alpha = 1, nlambda = 100, lambda = NULL,
    standardize = TRUE)

  estimator.BB(method = c(2, 3, 1), control = list(),
    quiet = FALSE,
    scoreFunctionCreator = scorefunctioncreator.default,
    ignore.error = FALSE)

  estimator.trymultiple(scoreFunctionCreator = scorefunctioncreator.default)

  scorefunctioncreator.default(Z, Y, link)
}
\arguments{
  \item{jac,global,xscalm}{See \code{\link{nleqslv}}.}

  \item{method}{See \code{\link{nleqslv}} /
  \code{\link{BBsolve}}.}

  \item{control}{See \code{\link{nleqslv}} /
  \code{\link{BBsolve}} / \code{\link{glm.fit}}.}

  \item{scoreFunctionCreator}{Function that will create the
  score vector function. This defaults to
  \code{scorefunctioncreator.default} and should be of the
  same form (and return a function of the same form as
  \code{scorefunctioncreator.default}).}

  \item{ignore.error}{Defaults to \code{FALSE}, but if
  \code{TRUE}, the errorcode of the \code{\link{nleqslv}}
  or \code{\link{BBsolve}} call will not be checked. Use at
  your own risk.}

  \item{alpha,nlambda,lambda,standardize}{See
  \code{\link{glmnet}}.}

  \item{quiet}{See \code{\link{BBsolve}}.}

  \item{Z}{Pseudo-observation design matrix}

  \item{Y}{Pseudo-observations.}

  \item{link}{Name of the link function.}
}
\value{
  These functions (\code{estimator.*}) each return a
  function themselves. The returned function should have
  three parameters (\code{startvalues}, a set of initial
  estimates of the parameters; \code{pfd}, an object of
  class \code{\link{pimfitdata}} ; \code{link}, the name of
  the link function) and should itself return a list of two
  items: \item{coefficients}{The parameter estimates}
  \item{morefitinfo }{Implementation specific information
  on the fit}

  For \code{scorefunctioncreator.default}: a function that
  takes a set of parameter estimates (\code{beta}) and
  calculates a set of values that should be solved for
  zero.
}
\description{
  Estimators for the PIM parameters
}
\details{
  These functions estimate the coefficients (/parameters)
  of the PIM by solving the (sometimes) nonlinear
  equations. Each calls upon another library for the
  solution and some may be more accurate / correct /
  performant depending on the specific model.

  \code{estimator.glm} returns the regular
  \code{\link{glm}} estimate (assuming the pseudo-
  observations are independent)

  \code{estimator.glmnet} returns the elsatic net penalized
  \code{\link{glmnet}} estimate (assuming, again, the
  pseudo-observations are independent)

  For the different implementations, \code{morefitinfo}
  contains: \enumerate{ \item \code{estimator.nleqslv} The
  return value of the \code{\link{nleqslv}} call \item
  \code{estimator.glm} The return value of the
  \code{\link{glm}} call \item{estimator.glmnet} The return
  value of the \code{\link{glmnet}} call, with some added
  items: \code{usedalpha},
  \code{usedfamily},\code{usedoffset} and
  \code{standardize} \item \code{estimator.BB} The return
  value of the \code{\link{BBsolve}} call }

  \code{estimator.trymultiple} is a special case that will
  simply try to use \code{nleqslv}, \code{} and \code{}
  with several parameters, hoping that one may lead to a
  fit. Although it provides the best chance of getting a
  fit, take care in assuring that the fit matches the
  covariance estimator! Also be aware that this may be
  slow.
}
\note{
  For \code{estimator.glmnet}, \code{coefficients} contains
  a sparse \code{\link{matrix}} holding the coefficient
  estimates and intercept (!) for all lambda values.
}
\keyword{coefficient}
\keyword{estimate}
\keyword{pim}

