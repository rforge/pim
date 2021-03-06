% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Getters_pim.R
\docType{methods}
\name{pim-getters}
\alias{pim-getters}
\alias{keep.data}
\alias{getters-pim}
\alias{fitted}
\alias{fitted,pim-method}
\title{Getters for slots of a pim object}
\usage{
keep.data(x)

fitted(object, ...)

\S4method{fitted}{pim}(object, ...)
}
\arguments{
\item{x}{a pim object}

\item{object}{a pim object}

\item{...}{arguments passed to other methods. Currently ignored.}
}
\value{
\code{keep.data()}: a single logical value indicating whether
the model matrix and pseudo responses were stored in the 
\code{\link{pim}} object.

\code{fitted()}: a numeric vector with the fitted
values for the pseudo-observations.
}
\description{
Getters for slots of a pim object
}
\examples{
data('FEVData')
themodel <- pim(FEV ~ Age + Height, data = FEVData)
keep.data(themodel)
fitted(themodel)

}
