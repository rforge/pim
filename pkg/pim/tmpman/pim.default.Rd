\name{pim.default}
\alias{pim.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pim.default(Z, PO, gL, gR, linkfunction, logical.formula, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{PO}{
%%     ~~Describe \code{PO} here~~
}
  \item{gL}{
%%     ~~Describe \code{gL} here~~
}
  \item{gR}{
%%     ~~Describe \code{gR} here~~
}
  \item{linkfunction}{
%%     ~~Describe \code{linkfunction} here~~
}
  \item{logical.formula}{
%%     ~~Describe \code{logical.formula} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Z , PO, gL, gR, linkfunction, logical.formula, ...)
{ 
	Z <- as.matrix(Z)
	PO <- as.numeric(PO)

	est <- pimEst(Z, PO, gL, gR, linkfunction)
	est$call <- match.call()
	class(est) <- "pim"
	est
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
