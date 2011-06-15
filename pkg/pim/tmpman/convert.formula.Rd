\name{convert.formula}
\alias{convert.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert.formula(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
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
function(formula, data)
{
	mf.tmp <- model.frame(formula = formula, data = data)	
	mf1 <- model.matrix(formula, data = data)
        mf <- as.matrix(mf1[,-1])	
        colnames(mf) <- colnames(mf1)[-1]
        mf.new <- as.data.frame(model.matrix(formula(paste(names(mf.tmp)[1],"~", paste( names(mf.tmp)[-1],collapse="+"))),mf.tmp))
	names.tmp <- names(mf.new)[-1]
	names.L <- paste(names.tmp,"L",sep="")
	names.R <- paste(names.tmp,"R",sep="")
	termsL <- termsR <- gsub(":", "*", colnames(mf))
	for(i in 1:length(names.tmp))
	{
		termsL <- sub(names.tmp[i], names.L[i],termsL)
		termsR <- sub(names.tmp[i], names.R[i],termsR)
	}
	new.formRHS <- paste(paste(paste("I(", termsR,"-" ,termsL,")",sep=""), collapse = "+"), "-1",sep="")
	new.formLHS <- paste("P(",names(mf.tmp)[1],"L", " < ", names(mf.tmp)[1],"R)",sep="")
	new.form <- paste(new.formLHS, new.formRHS, sep="~")
	data.tmp <- data.frame(mf.tmp[,1],mf.new)[,-2]
        names(data.tmp)[1] <- names(mf.tmp)[1]
	return(list(new.formula = new.form, reduced.data =data.tmp))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
