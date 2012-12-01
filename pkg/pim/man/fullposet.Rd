\name{fullposet}
\alias{forcedcolorderonewayposet}
\alias{fullposet}
\alias{oldpimposet}
\alias{oldpimposetbft}
\alias{onewayposet}
\alias{pairwiseposet}
\alias{poset}
\title{Get a set of pseudo-observation observation indices}
\usage{
  fullposet(data, formula, verbosity = 0)

  pairwiseposet(data, formula, verbosity = 0)

  onewayposet(data, formula, verbosity = 0)

  forcedcolorderonewayposet(columnnames = NULL)

  oldpimposet(data, formula, verbosity = 0)

  oldpimposetbft(data, formula, verbosity = 0)
}
\arguments{
  \item{data}{Context where the formula \code{formula} is
  to be interpreted}

  \item{formula}{Original formula}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}

  \item{columnnames}{column names that will be used to
  imply order.}
}
\value{
  List with 2 items: \item{data}{Similar to the passed
  along \code{data}, but may contain fewer rows.}
  \item{poset }{Matrix of two columns indicating what the
  original observation number is for the left and right
  real observation in the pseudo-observation.}
}
\description{
  Get a set of pseudo-observation observation indices
}
\details{
  The provided implementations differ as follows:
  \enumerate{ \item \code{fullposet} Contains all
  combinations of rowindices. \item \code{pairwiseposet}
  Check that predicting variables can be ordered, and
  select only combinations where the predictors are bigger
  on the right side. \item \code{onewayposet} Similar to
  \code{pairwiseposet}, but simply uses row index. \item
  \code{forcedcolorderonewayposet} First reorders the data
  based on a set of given column names. Note: the other
  functions here are to be passed along as
  \code{poset=fullposet}, this one needs
  \code{poset=forcedcolorderonewayposet(c("col1", "col2"))}
  \item \code{oldpimposet} and \code{oldpimposetbft} Sorts
  the data according to all predictors, then does
  oneway-style. These are mainly provided for comparison to
  a previous implementations of pim. }
}
\examples{
set.seed(1)
	dta<-data.frame(y=runif(6), x=ordered(sample(3, 6, replace=TRUE)))
	fullposet(dta, y~x)$poset
	pairwiseposet(dta, y~x)$poset
	onewayposet(dta, y~x)$poset
}
\seealso{
  \code{\link{pim}}
}
\keyword{fit}
\keyword{pim}
\keyword{poset}

