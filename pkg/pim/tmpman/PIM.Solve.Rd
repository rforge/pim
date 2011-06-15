\name{PIM.Solve}
\alias{PIM.Solve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PIM.Solve(Z, PO, linkfunction)
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
	if (!exists("nleqslv")) require(nleqslv)
	if(linkfunction == "logit" | linkfunction == "probit") 
		fit.x <- nleqslv(rep(0,ncol(Z)), PIM.ScoreFunction(Z, PO, linkfunction), Z=Z, PO=PO)$x
	else stop("linkfunction must be 'logit' or 'probit'.")
	names(fit.x) <- colnames(Z) 
	return(fit.x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
