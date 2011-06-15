\name{PIM.Data}
\alias{PIM.Data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PIM.Data(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function(data)
{
	n <- nrow(data)
	d <- ncol(data) - 1
	left.tmp <- unlist(mapply(seq,rep(1,(n-1)), 1:(n-1)))
	right.tmp <- unlist(mapply(rep, 2:n, 1:(n-1)))
	data.tmp <- as.data.frame(matrix(nrow=n*(n-1)/2, ncol=(1+2*d+2)))
	names(data.tmp) <- c("PO",paste(names(data)[-1],"L",sep=""),paste(names(data)[-1],"R",sep=""),"groupL","groupR")
	data.tmp[,1] <- c(data[left.tmp,1] < data[right.tmp,1]) + 0.5*c(data[left.tmp,1] == data[right.tmp,1])
	data.tmp[,2:(d+1)] <- data[left.tmp,-1]
	data.tmp[,(d+2):(2*d+1)] <- data[right.tmp,-1]
	data.tmp[,2*d+2] <- left.tmp
	data.tmp[,2*d+3] <- right.tmp
	return(data.tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
