\name{pim}
\alias{pim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting Probabilistic Index Models
}
\description{
'pim' is used to fit probabilistic index models. 
}
\usage{
pim(formula , data, linkfunction = "logit", boolean.formula = 'FALSE')
pim(Z, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
a numeric design matrix for the model.
}   
  \item{formula}{
an object of class \code{formula}. Similar use as in \code{lm}. 
}
  \item{data}{
a data frame. 
}
  \item{linkfunction}{
a link function which can be logit or probit. 
}
   \item{boolean.formula}{
logical. If 'FALSE' formula will be automatically converted to PIM-like formula. If 'TRUE' the user can specify PIM-like formula directly. 
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of the class \code{pim} with elements
\item{coefficients}{ a named vector of coefficients }
\item{vcov}{ covariance matrix of coefficients }
\item{fitted.values}{ fitted values }
}
\references{
Thas, O. De Neve, J. Clement, L. and Ottoy, J.P. Probabilistic Index Models. Submitted. 
}
\author{
Jan De Neve
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(1)
data.tmp <- data.frame(Y = rnorm(20), X = rnorm(20))
pim.fit <- pim(formula = Y ~ X, data = data.tmp)
summary(pim.fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{probabilistic index model}
\keyword{regression}% __ONLY ONE__ keyword per line
