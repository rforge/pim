\name{PIM.SandwichEstimator}
\alias{PIM.SandwichEstimator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PIM.SandwichEstimator(U, U.diff, g1, g2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{U}{
%%     ~~Describe \code{U} here~~
}
  \item{U.diff}{
%%     ~~Describe \code{U.diff} here~~
}
  \item{g1}{
%%     ~~Describe \code{g1} here~~
}
  \item{g2}{
%%     ~~Describe \code{g2} here~~
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
function(U, U.diff, g1, g2)
{
	if (!exists("rowsum")) require(survival4)
	usum1.tmp <- rowsum(U,g1,reorder=F)
	usum2.tmp <- rowsum(U,g2,reorder=F)
	usum1  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum1.tmp),0)
	usum2  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum2.tmp),0)
	usum1 [unique(g1),] <- usum1.tmp; usum2[unique(g2),] <- usum2.tmp
	utu<-(t(usum1)\%*\%usum1)+t(usum2)\%*\%usum2  + (t(usum1)\%*\%usum2) + (t(usum2)\%*\%usum1) -t(U)\%*\%U
        U.diff.inv <- solve(U.diff)
	return(U.diff.inv\%*\%utu\%*\%U.diff.inv)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
