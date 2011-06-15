\name{PIM.ScoreVector}
\alias{PIM.ScoreVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PIM.ScoreVector(Zbeta, Z, PO, linkfunction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Zbeta}{
%%     ~~Describe \code{Zbeta} here~~
}
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
function(Zbeta, Z, PO, linkfunction)
{
	Zbeta <- c(Zbeta)
	PO <- c(PO)
	if(linkfunction == "probit")
	{
		fv <- pnorm(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI)
		m.d <- dnorm(Zbeta)
		m.dd <- -m.d*Zbeta 
		res <- PO-fv
		U <- Z*m.d*res/var.PI
		U.diff <- t(Z)\%*\%(Z*c((var.PI*(m.dd*res - m.d^2) - res*m.d^2*(1-2*fv))/var.PI^2))
	}else
	{
		fv <- plogis(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI)
		U <- Z*c(PO-fv)
		U.diff <- -t(Z)\%*\%(Z*c(var.PI))
	}
	return(list(U, U.diff, fv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
