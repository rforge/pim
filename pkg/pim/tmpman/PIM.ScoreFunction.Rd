\name{PIM.ScoreFunction}
\alias{PIM.ScoreFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PIM.ScoreFunction(Z, PO, linkfunction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{PO}{
%%     ~~Describe \code{PO} here~~
}
  \item{linkfunction}{
%%     ~~Describe \code{linkfunction} here~~
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
function(Z, PO, linkfunction)
{
	if(linkfunction == "probit"){
		U.func <- function(beta, Z, PO){ 
		Zbeta <- c(Z\%*\%beta)
		colSums(Z*dnorm(Zbeta)*c(PO - pnorm(Zbeta))/c(pnorm(Zbeta)*(1-pnorm(Zbeta))))}
	}else
	{
		U.func <- function(beta, Z, PO) colSums(Z*c(PO - plogis(Z\%*\%beta)))
	}
	return(U.func)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
