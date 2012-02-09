%- JM Corrected abs for \left| and \right|

\name{pim}
\alias{pim}
\alias{pim.default}
\alias{pim.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting Probabilistic Index Models
}
\description{
'pim' is used to fit probabilistic index models. 
}
\usage{
pim(Z, ...)
\method{pim}{formula}(formula , data = list(), linkfunction = "logit", boolean.formula = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{
 a numeric design matrix for the model.
}   
  \item{formula}{
an object of class \code{formula}. Similar use as in \code{lm} and \code{glm}, except when \code{boolean.formula = 'TRUE'}, see \code{boolean.formula} for details.
}
  \item{data}{
a data frame. 
}
  \item{linkfunction}{
a link function which can be \code{"logit"} or \code{"probit"}. 
}
   \item{boolean.formula}{
logical value. If 'FALSE' the \code{lm}-type formula will be automatically converted to a valid formula for a probabilistic index model. If 'TRUE' the user can specify the probabilistic index model formula directly. See Details for more information.
}
  \item{\dots}{
  further arguments passed to or from other methods.

}
}
\details{
Let \eqn{(Y,X)} and \eqn{(Y',X')} be i.i.d., then a probabilistic index model is defined as
\deqn{
P(Y \leq Y'| X, X') = g^{-1}(Z^{T} \beta),
}
where \eqn{Z} is a function of the predictors \eqn{X} and \eqn{X'} and \eqn{g()} is a link function. The model is fitted according to the lexicographical order restriction. 

If \code{boolean.formula = 'FALSE'}, \eqn{Z} is the difference in predictors \eqn{X} and \eqn{X'}, i.e. \eqn{Z = X' - X}.  Model 1 of the Examples corresponds to
\deqn{
logit{P(Y \leq Y'| X, X')} = \beta_1(X'_1 - X_1) + \beta_2(X'_2 - X_2) + \beta_3 (X'_1*X'_2 - X_1*X_2).
}

If \code{boolean.formula = 'TRUE'}, the user can specify \eqn{Z}. The predictor corresponding to the left response in \eqn{P(Y \leq Y'| X, X')} (thus \eqn{X}) is denoted by \eqn{XL} (L from left). Similar the predictor corresponding to the right response in \eqn{P(Y \leq Y'| X, X')} (thus \eqn{X'}) is denoted by \eqn{XR} (R from right). Use the \code{I()}-function for specific functions. Model 2 of the Examples corresponds to
\deqn{
logit{P(Y \leq Y'| X, X')} = \beta_1\frac{\sqrt{X'_1 - X_1}}{\left|{X'_1 + X_1}\right|} + \beta_2(X'_2 - X_2).
}

}
\value{
An object of the class \code{pim} with elements
\item{coefficients}{ a named vector of coefficients }
\item{vcov}{ covariance matrix of coefficients }
\item{fitted.values}{ fitted values }
\item{formula}{ formula statement}
\item{linkfunction}{ link function}
\item{PIMdata}{ transformed dataset with pseudo-observations (\code{PO}), corresponding covariates and group memberships }
\item{data}{ original dataset restricted to the variables which are specified in the \code{formula} argument } 
}
\references{
Thas, O. De Neve, J. Clement, L. and Ottoy, J.P. (2012) Probabilistic Index Models (with discussion). Journal of the Royal Statistical Society - Series B, 74:1-29.
}
\author{
Jan De Neve (JanR.DeNeve@Ugent.be)
}
\note{
More information can be found in the vignette, see \code{vignette('pim')}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(1)
X1 <- rnorm(20)
X2 <- runif(20)
Y <- 2*X1 + 3*X2 + rnorm(20)
data.tmp <- data.frame(Y, X1, X2)

## Model 1 ##
pim.fit1 <- pim(formula = Y ~ X1*X2, data = data.tmp)
summary(pim.fit1)


## Model 2 ##
pim.fit2 <- pim(formula = Y ~ I((X1R - X1L)/abs(X1R + X1L)) + I(X2R - X2L) - 1, data = data.tmp, boolean.formula = TRUE)
summary(pim.fit2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{probabilistic index model}
\keyword{regression}% __ONLY ONE__ keyword per line
