\name{pim.fit}
\alias{pim.fit}
\title{Actually fit the pim}
\usage{
  pim.fit(pfd,
    link = c("logit", "identity", "probit", "inverse", "1/mu^2", "log"),
    estimator = estimator.nleqslv(),
    varianceestimator = varianceestimator.sandwich(),
    verbosity = 0, nicenames = TRUE)
}
\arguments{
  \item{pfd}{Object of class \code{\link{pimfitdata}}}

  \item{link}{Name of the link function (defaults to
  \code{"identity"})}

  \item{estimator}{Function like the result of
  \code{\link{estimator.nleqslv}()} (the default). See
  there to find the required form of this function or some
  provided alternatives.}

  \item{varianceestimator}{Function like the result of
  \code{\link{varianceestimator.sandwich}()} (the default).
  See there to find the required form of this function or
  some provided alternatives.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{nicenames}{Defaults to \code{TRUE}: try to make the
  column names more readable.}
}
\value{
  List with (some of) the items:
  \item{coefficients}{Parameter estimates. See
  \code{\link{estimator.nleqslv}}} \item{morefitinfo
  }{Parameter estimation info. See
  \code{\link{estimator.nleqslv}}}
  \item{fitted.values}{Predicted pseudo-observation
  values.} \item{vcov }{Covariance estimates. See
  \code{\link{varianceestimator.sandwich}}}
  \item{morevarfitinfo }{Covariance estimation info. See
  \code{\link{varianceestimator.sandwich}}}
}
\description{
  Actually fit the pim from prepared data
}
\details{
  The presence or absence or form of the items in the
  return value depends mainly upon the passed along
  \code{estimator} and \code{varianceestimator}.

  If \code{estimator} fails, no attempt is taken to
  estimate the covariances (and the matching items are not
  in the return value). If \code{varianceestimator} was
  \code{NULL}, the items are simply set to \code{NULL}.

  If \code{varianceestimator} is executed, but it fails,
  the error is saved in an additional item
  \code{varestimationerror}.
}
\note{
  Typically, you will not use this method directly
  (similarly as \code{\link{glm.fit}} for
  \code{\link{glm}})
}
\examples{
set.seed(1)
	iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
	poset<-fullposet(iris, out~Sepal.Length)$poset
	pfd<-pim.fit.prep(out~Sepal.Length, data=iris, poset=poset)
	pf<-pim.fit(pfd)
}
\seealso{
  \code{\link{estimator.nleqslv}}
  \code{\link{varianceestimator.sandwich}}
}
\keyword{fit}
\keyword{pim}

