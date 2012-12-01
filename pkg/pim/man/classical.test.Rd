\name{classical.test}
\alias{classical.test}
\title{PIM implementations of distributionfree tests}
\usage{
  classical.test(test = c("WilcoxonMannWhitney", "KruskalWallis", "MackSkillings", "BrownHettmansperger", "JonckheereTerpstra", "MackWolfe"),
    data, out, group, block,
    varianceestimator = varianceestimator.H0(),
    alternative = c("two.sided", "greater", "less"),
    levelP, verbosity = 0)
}
\arguments{
  \item{test}{Type of "classical" distribution free test to
  perform.}

  \item{data}{Context where \code{out}, \code{group} and
  \code{block} are to be interpreted}

  \item{out}{Column of \code{data} that holds the outcomes
  (responses)}

  \item{group}{Column of \code{data} that holds the
  predicting variable}

  \item{block}{Column of \code{data} that holds the
  blocking variable. This can be left out if no blocking is
  present}

  \item{varianceestimator}{Function like the result of
  \code{\link{varianceestimator.sandwich}()} (the default).
  The default (\code{\link{varianceestimator.H0}}) will
  provide the classical test statistic. With the Sandwich
  estimator, you can achieve a Wald-type test.}

  \item{alternative}{As for other tests}

  \item{levelP}{The level of the grouping variable that is
  the "top" of the umbrella (only relevant for
  \code{test="MackWolfe"}).}

  \item{verbosity}{The higher this value, the more levels
  of progress and debug information is displayed (note: in
  R for Windows, turn off buffered output)}
}
\value{
  a list holding the following items: \item{statistic}{The
  test statistic.} \item{p.value}{p-value for the test.}
  \item{df}{Degress of freedom (if relevant, otherwise
  \code{NA}).} \item{conversion}{Function to convert the
  pim estimates to the test statistic.}
}
\description{
  PIM implementations of distributionfree tests
}
\note{
  These functions are merely provided for comparison. The
  pim function supports generic ways of estimating these
  values. For generosity, the parameters to all of them are
  the same, though some are not relevant (e.g. \code{block}
  for WMW).
}
\keyword{fit}
\keyword{legacy}
\keyword{pim}

