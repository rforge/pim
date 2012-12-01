\name{IPI}
\alias{IPI}
\title{Probabilistic Index / Indicator for sets of variables}
\usage{
  IPI(yi, yj, method = c("pairwise", "paired"),
    res = c("sum", "mean", "all", "sumcount"))
}
\arguments{
  \item{yi,yj}{Variables for which the I/PI will be
  calculated. \code{yi} will always be at the left hand
  side of the unequality.}

  \item{method}{If \code{"pairwise"} (the default), then
  all combinations of items in \code{yi} and \code{yj} are
  used. Otherwise both are taken in the order specified
  (implying equal lengths)}

  \item{res}{Calculate the \code{"sum"} (default),
  \code{"mean"} (i.e. probabilistic index) of the
  combinations, or simply return \code{"all"} partial
  results. Finally, \code{"sumcount"} returns both the sum
  and the count.}
}
\value{
  If \code{res} is \code{"sum"} or \code{"mean"}, a single
  value, otherwise a \code{data.frame} with the follwing
  columns \item{yi}{Left hand side of the combination.}
  \item{yj}{Right hand side of the combination.}
  \item{pi}{The probabilistic indicator for this
  combination.}
}
\description{
  Probabilistic Index / Indicator for sets of variables
}
\keyword{index}
\keyword{indicator}
\keyword{pim}
\keyword{probabilistic}

