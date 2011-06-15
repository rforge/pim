\name{plot.pim}
\alias{plot.pim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.pim(object, G = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{G}{
%%     ~~Describe \code{G} here~~
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
function(object, G=10, ...)
{
	fv.tmp <- fitted.values(object)
	groups.tmp <- cut(fv.tmp, quantile(fv.tmp, probs =   seq(0,1, 1/G)), include.lowest = T) 
	emp.prob <- tapply(object$PIMdata$PO, groups.tmp, mean)
	model.prob <- tapply(fv.tmp, groups.tmp, mean)
	plot(emp.prob, model.prob, xlim=c(min(emp.prob),max(emp.prob)) ,ylim=c(min(model.prob),max(model.prob)),main="",lwd=2,xlab="",ylab="")
	title(xlab="Empirical PI",ylab="Model PI",font.lab=1,cex.lab=1,main="Goodness-of-fit")
	abline(0,1,col=2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
