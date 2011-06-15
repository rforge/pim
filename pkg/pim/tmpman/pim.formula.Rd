\name{pim.formula}
\alias{pim.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pim.formula(formula, data = list(), linkfunction = "logit", boolean.formula = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{linkfunction}{
%%     ~~Describe \code{linkfunction} here~~
}
  \item{boolean.formula}{
%%     ~~Describe \code{boolean.formula} here~~
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
function(formula, data = list(), linkfunction = "logit", boolean.formula = FALSE,  ... )
{
	if(!boolean.formula) 
	{
		tr.tmp <- convert.formula(formula, data)
		formula <- formula(tr.tmp$new.formula)
		data <- tr.tmp$reduced.data
	}else
	{
		data <- data[, c(grep(formula[[2]], names(data)), grep(formula[[2]], names(data), invert = TRUE))]
	}	
	PIM.tmp <- PIM.model.matrix(formula, data) 
	data.tmp <- PIM.tmp$data.tmp

	est <- pim.default(Z = PIM.tmp$Z, PO = data.tmp$PO, gL = data.tmp$groupL, gR = data.tmp$groupR, linkfunction = linkfunction,... )
	est$call <- match.call()
	est$formula <- formula
        est$linkfunction <- linkfunction
	est$PIMdata <- data.tmp
	est$data <- data
        est	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
