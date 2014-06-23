\name{add.varianceestimate}
\alias{add.varianceestimate}
\title{Add (co)variance estimates to a PIM}
\usage{
  add.varianceestimate(object,
    varianceestimator = varianceestimator.sandwich(),
    estimator = estimator.nleqslv(), verbosity = 0)
}
\arguments{
  \item{object}{\code{\link{pim}} object}

  \item{estimator}{Function like the result of
  \code{\link{estimator.nleqslv}()} (the default). Note:
  this should be the same as was originally used for
  creating the PIM.  This cannot be retrieved from
  \code{object} as saving it would have a big impact.}

  \item{varianceestimator}{Function like the result of
  \code{\link{varianceestimator.sandwich}()} (the default).
  See there to find the required form of this function or
  some provided alternatives.}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  An object of class \code{\link{pim}}. The covariance
  related items are replaced in \code{object}, or added.
  Typically, these are \code{vcov} and
  \code{morevarfitinfo}
}
\description{
  Add (co)variance estimates to a PIM
}
\details{
  The result should be the same as you would have gotten if
  you passed along the \code{varianceestimator} to the
  original \code{\link{pim}} call.  Important: this
  function will only work for PIMs where you specified
  \code{keep.data=TRUE}.
}
\note{
  Most variance estimators do not actually use the
  \code{estimator}. The only one know at the time of this
  writing is \code{\link{varianceestimator.bootstrap}}. So
  in practice this parameter can probably be mostly
  ignored.
}
\examples{
set.seed(1)
iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
iris$xord<-as.ordered(iris$Species)
pima<-pim(out~Sepal.Length, data=iris, keep.data=TRUE, link="identity")
pimb<-add.varianceestimate(pima, varianceestimator=varianceestimator.H0())
}
\seealso{
  \code{\link{estimator.nleqslv}}
  \code{\link{varianceestimator.sandwich}}
  \code{\link{pim}}
}
\keyword{pim}
\keyword{variance}

