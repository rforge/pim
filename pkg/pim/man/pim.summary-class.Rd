% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pim.summary-class.R
\docType{class}
\name{pim.summary-class}
\alias{pim.summary-class}
\alias{pim.summary}
\title{Class pim.summary}
\description{
This class contains the summary information from a probabilistic index
model , and is created by using the function \code{\link{summary}}
on an object of the \code{\link{pim-class}}.
}
\details{
The class \code{pim.summary} can be treated like a matrix to get out
the coefficients, standard errors, Z values and/or p values.
}
\section{Slots}{

\describe{
\item{\code{formula}}{contains an object of the class \code{\link{pim.formula}}
containing the model fitted.}

\item{\code{model}}{a character vector describing the type of model. See also the argument \code{model} of the function \code{\link{pim}}}

\item{\code{link}}{a character value that contains the link. See also
the argument \code{link} of the function \code{\link{pim}}}

\item{\code{coef}}{a numeric vector with the coefficients}

\item{\code{se}}{a numeric vector with the standard errors for the coefficients}

\item{\code{zval}}{a numeric vector containing the Z values for the coefficients,
testing whether the coefficient differs significantly from 0.}

\item{\code{pr}}{a numeric vector containing the related p-values for the
coefficients.}

\item{\code{h0}}{a numeric value or a numeric vector containing
the null hypothesis. See the argument at \code{\link{summary.pim}}}
}}

\seealso{
\code{\link{pim}} for more info on how to construct the model
and \code{\link{summary.pim}} for the constructor.
}
