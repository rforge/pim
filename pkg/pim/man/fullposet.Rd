\name{fullposet}
\alias{forcedcolorderonewayposet}
\alias{forcedposet}
\alias{fullposet}
\alias{lexiposet}
\alias{noselfposet}
\alias{onewayposet}
\alias{poset}
\title{Get a set of pseudo-observation observation indices}
\usage{
  fullposet(data, formula, verbosity = 0)

  noselfposet(data, formula, verbosity = 0)

  lexiposet(data, formula, verbosity = 0)

  onewayposet(data, formula, verbosity = 0)

  forcedcolorderonewayposet(columnnames = NULL)

  forcedposet(diffcols = NULL, LLessRcols = NULL,
    RLessLcols = NULL, no.equals = TRUE)
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

  \item{diffcols}{column names that will be used to imply
  order.}

  \item{LLessRcols}{column names where you want the left
  observation to be smaller.}

  \item{RLessLcols}{column names where you want the right
  observation to be smaller}

  \item{no.equals}{if \code{TRUE} (the default), rows that
  have equal values in the \code{LLessRcols} or
  \code{RLessLcols} columns are excluded from being
  combined.}
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
  combinations of rowindices. \item \code{noselfposet} The
  same as fullposet, but excluding the rowcombinations with
  identical indexes. \item \code{lexiposet} Check that
  predicting variables can be ordered, and select only
  combinations where the predictors are bigger on the right
  side. \item \code{onewayposet} Similar to
  \code{lexiposet}, but simply uses row index. \item
  \code{forcedcolorderonewayposet} First reorders the data
  based on a set of given column names. Note: the other
  functions here are to be passed along as
  \code{poset=fullposet}, this one needs
  \code{poset=forcedcolorderonewayposet(c("col1", "col2"))}
  \item \code{forcedposet} Starts from all observations,
  but excludes the ones where \itemize{ \item the
  combinations of the columns in \code{diffcols} are the
  same in left and right observation \item the combinations
  of the columns in \code{LLessRcols} in the left
  observation are greater than the one in the right
  observation (equals are dropped too if
  \code{no.equals=TRUE}) \item the combinations of the
  columns in \code{RLessLcols} in the right observation are
  greater than the one in the left observation (equals are
  dropped too if \code{no.equals=TRUE}) } As a side effect,
  the data is also sorted wrt the \code{LLessRcols}
  (ascending) and then according to the \code{RLessLcols}
  (descending). \item \code{oldpimposet} and
  \code{oldpimposetbft} Sorts the data according to all
  predictors, then does oneway-style. These are mainly
  provided for comparison to a previous implementations of
  pim. }
}
\examples{
set.seed(1)
	dta<-data.frame(y=runif(6), x=ordered(sample(3, 6, replace=TRUE)))
	fullposet(dta, y~x)$poset
	lexiposet(dta, y~x)$poset
	onewayposet(dta, y~x)$poset
}
\seealso{
  \code{\link{pim}}
}
\keyword{fit}
\keyword{pim}
\keyword{poset}

