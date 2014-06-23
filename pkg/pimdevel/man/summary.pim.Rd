\name{summary.pim}
\alias{design.matrix}
\alias{design.matrix.pim}
\alias{designcols}
\alias{designcols.pim}
\alias{outcomeformula}
\alias{outcomeformula.pim}
\alias{print.summary.pim}
\alias{print.summary.pimo}
\alias{responses}
\alias{responses.pim}
\alias{summary.pim}
\alias{summary.pim-class}
\alias{vcov.pim}
\title{Plot very basic G.O.F. for a pim}
\usage{
  \method{summary}{pim}(object,...)

  \method{print}{summary.pim}(x,...)

  \method{vcov}{pim}(object,...)

  design.matrix(object, ...)

  \method{design.matrix}{pim}(object,...)

  responses(object, ...)

  \method{responses}{pim}(object,...)

  designcols(object, ...)

  \method{designcols}{pim}(object,...)

  outcomeformula(object, ...)

  \method{outcomeformula}{pim}(object,...)
}
\arguments{
  \item{object}{\code{\link{pim}} object.}

  \item{\dots}{Ignored currently.}

  \item{x}{\code{summary.pim} object.}
}
\value{
  For \code{summary.pim}: an object of class
  \code{summary.pim}. It holds the original call in an item
  call, and has an additional item coefficients, that is a
  matrix holding columns: \item{Estimate}{The coefficient
  estimate.} \item{Std. Error}{Their standard error.}
  \item{Z value}{The standardized value (Test statistic for
  true coefficient zero).} \item{Pr(>|z|)}{The p-value.}

  For \code{print.summary.pim}: invisibly returns \code{x}

  For \code{vcov.pim}: a (co)variance matrix

  For \code{design.matrix}: The design matrix (of
  pseudo-observations)

  For \code{responses}: The true responses
  (pseudo-observations)

  For \code{designcols}: The orginal column names of the
  design matrix.  These typically also contain the part of
  the formula used to build them, so indicate their source.

  For \code{outcomeformula}: The left hand side of the
  formula used to.  create the design matrix (i.e. to
  generate the pseudo-outcomes).
}
\description{
  Plot very basic Goodness Of Fit for a pim
}
\examples{
set.seed(1)
	myiris<-iris
	myiris$xord<-ordered(sample(letters[1:3], nrow(myiris), replace=TRUE))
	myiris$out<-runif(nrow(myiris))

	irisprt<-myiris[sample(nrow(myiris), 10),] #10 random rows from iris
	pim1<-pim(out~Sepal.Length, data=irisprt, link="logit")
	summary(pim1)
	vcov(pim1)
	design.matrix(pim1)
	responses(pim1)
}

