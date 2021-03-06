% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poset.R
\docType{methods}
\name{poset}
\alias{poset}
\alias{poset,pim.environment-method}
\alias{poset,pim.poset-method}
\alias{poset,environment-method}
\alias{poset,pim-method}
\alias{poset,pim.formula-method}
\title{Extract the poset as a matrix or list}
\usage{
poset(x, ...)

\S4method{poset}{pim.environment}(x, ...)

\S4method{poset}{pim.poset}(x, as.list = FALSE)

\S4method{poset}{environment}(x, as.list = FALSE)

\S4method{poset}{pim}(x, ...)

\S4method{poset}{pim.formula}(x, ...)
}
\arguments{
\item{x}{an object of class \code{\link{pim.environment}},
\code{\link{pim.formula}}, \code{\link{pim}} 
or \code{\link{pim.poset}}, or an environment derived from
either object.}

\item{...}{arguments passed to other methods. Currently ignored.}

\item{as.list}{a logical value indicating whether the poset should
be returned as list or as a matrix. Defaults to FALSE, which returns
a matrix}
}
\value{
When x contains a poset, either a matrix or a list 
(when \code{as.list} is \code{TRUE}) with the indices that
make up the poset. If there's no poset, the function returns
a missing value.

The returned matrix hax 2 columns, each named after the 
respective poset function. In case a list is requested, the function
returns a named list with 2 elements, each element containing the
indices related to the poset function of the same name 
(either \code{\link{L}} or \code{\link{R}}).
}
\description{
This function allows you to extract the poset from either a 
\code{\link{pim.environment}} or a \code{\link{pim.poset}} object.
The poset can be extracted as a matrix or a list.
}
\examples{
data(DysData)
DysPimEnv <- new.pim.env(DysData)
poset(DysPimEnv)

}
