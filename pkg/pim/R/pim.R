#' Fit a PIM
#' 
#' Fit a PIM
#' 
#' @aliases pim pim-class glmnetpim glmnetpim-class
#' 
#' @param formula Original formula
#' @param data Context where the formula \code{formula} is to be interpreted
#' @param link Name of the link function (defaults to \code{"logit"})
#' @param blocking.variables Character vector holding column names that hold blocking
#' 	variables.
#' @param poset Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation. Alternatively, can
#' 	be a function like \code{\link{fullposet}} and similar.
#' @param leftsuffix,rightsuffix Suffixes that will be added to the 'left' and 'right'
#' 	observation's column name in the pseudo-observation. Note: no checking is done that 
#' 	these suffixes are safe, so the wrong suffixes may lead to unexpected behaviour.
#' @param interpretation If \code{"marginal"} parts of the formula are converted to imply
#' 	 marginal pim modeling (see e.g. \code{\link{Mainreplacetext}}). If it is \code{"difference"}
#' 	 (the default), then the design matrix of the PIM is the difference of the
#' 	design matrices of each part of the pseudo-observations. A popular option is
#' 	\code{"regular"}, which will interpret unaltered columns as differences. A new option
#' 	is \code{"symmetric"}, which works the same as \code{"regular"}, but will enforce
#' 	the symmetry condition by making the sign switch when changing the order (typically,
#' 	this is achieved by subtracting the inverse for each dummy).
#' @param na.action Defaults to \code{\link{na.fail}}: handles missing data in \code{data}.
#' @param estimator Function like the result of \code{\link{estimator.nleqslv}()} (the 
#' 	default). See there to find the required form of this function or some provided 
#' 	alternatives.
#' @param varianceestimator Function like the result of \code{\link{varianceestimator.sandwich}()}
#' 	(the default). See there to find the required form of this function or some 
#' 	provided alternatives.
#' @param lhs \code{"PO"}, \code{"<"} or \code{"<="}: Unequality used for the lefthandside of 
#' 	the formula. The default (\code{"PO"}) is the normal probabilistic index.
#' @param keep.data If \code{TRUE} (not the default), the data and design matrices are maintained
#' 	in the return value 
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param nicenames Defaults to \code{TRUE}: try to make the column names more readable.
#' @param interactions.difference If \code{TRUE} (note that the default is  
#' 	\code{interpretation!="marginal"}) interaction terms will be interpreted as the  
#' 	differences of the onesided interaction terms (if this is possible at all). This is  
#' 	unsupported if \code{unsupported if} is \code{"marginal"}. Some special interaction 
#' 	terms with calculated columns may lead to unexpected behaviour.
#' @param extra.nicenames Should be a \code{data.frame} containing two character columns: 
#' 	\code{org} and \code{nice}. For each "constructed" column name, provide a nicer one,  
#' 	that will make the results more readable. You may also use parts of constructed column 
#' 	names. Note: make sure to use \code{stringsAsFactor=FALSE} when creating the \code{data.frame}.
#' @return An object of class \code{pim},which is a list with items:
#' \item{coefficients}{Parameter estimates. See \code{\link{estimator.nleqslv}}} 
#' \item{morefitinfo }{Parameter estimation info. See \code{\link{estimator.nleqslv}}} 
#' \item{vcov }{Covariance estimates. See \code{\link{varianceestimator.sandwich}}} 
#' \item{morevarfitinfo }{Covariance estimation info. See \code{\link{varianceestimator.sandwich}}}
#' \item{varestimationerror }{Only present if covariance estimation failed. See \code{\link{pim.fit}}}
#' \item{call }{Call used when running this function.}
#' \item{formula }{Passed in formula.}
#' \item{link }{Name of the link function (defaults to \code{"identity"})}
#' \item{fitted.values }{Predicted pseudo-observation values.}
#' \item{pfd }{Object of class \code{\link{pimfitdata}}. If \code{keep.data} was \code{TRUE}, the 
#' 	\code{X} and \code{Y} items are set to \code{NULL}.}
#' \item{data }{Passed in \code{data} (may be altered by \code{poset}). Only present if 
#' 	\code{keep.data} was \code{TRUE}}
#' @details Some of the items in the return value may be missing if parts of the estimation
#' 	fail. See \code{\link{pim.fit}} for this.
#' 	
#' 	If \code{estimator} was \code{estimator.glmnet}, The return value gets as an additional class
#' 		\code{"glmnetpim"}.
#' 		
#' 	For more details on how to use this function and its parameters, we refer the reader to 
#' 	\code{vignette("pim")}.
#' @seealso \code{\link{estimator.nleqslv}} \code{\link{varianceestimator.sandwich}}
#' @keywords pim fit
#' @examples set.seed(1)
#' iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
#' iris$xord<-as.ordered(iris$Species)
#' pima<-pim(out~Sepal.Length, data=iris, link="logit", interpretation="regular")
#' pimb<-pim(out~I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) ), data=iris, 
#' 	link="logit", interpretation="regular", extra.nicenames=data.frame(
#' 		org="I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) )", 
#' 		nice="Sepal.Length.WDiff", stringsAsFactors=FALSE))
#' pimc<-pim(out~O(xord), data=iris, link="logit", interpretation="regular")
#' @export
pim<-function(formula, data, link=c("logit", "identity", "probit", "inverse", "1/mu^2", "log"), 
							blocking.variables=character(),
							poset=fullposet, leftsuffix="_L", rightsuffix="_R", 
							interpretation=c("difference", "regular", "marginal", "symmetric"), na.action=na.fail,
							estimator=estimator.nleqslv(), varianceestimator=varianceestimator.sandwich(), 
							lhs=c("PO", "<", "<="), keep.data=FALSE, verbosity=0, 
							nicenames=TRUE, interactions.difference=(interpretation!="marginal"),
							extra.nicenames=data.frame(org=character(), nice=character(), stringsAsFactors=FALSE))
{
	this.call<-match.call()
	link<-match.arg(link)
	if(is.function(poset))
	{
		poset<-poset(data, formula, verbosity=verbosity-1)
		data<-poset$data
		poset<-poset$poset
	}
	if(verbosity>0) cat("Get design matrix and converted formula\n")
	pfd<-pim.fit.prep(formula=formula, data=data, blocking.variables=blocking.variables, poset=poset, leftsuffix=leftsuffix, rightsuffix=rightsuffix, 
										interpretation=interpretation, na.action=na.action, lhs=lhs, verbosity=verbosity-1, nicenames=nicenames,
										interactions.difference=interactions.difference, extra.nicenames=extra.nicenames)
	if(verbosity>0) cat("Coefficient and covariance estimates\n")
	actualfit<-pim.fit(pfd=pfd, link=link, estimator=estimator, varianceestimator=varianceestimator, verbosity=verbosity-1, nicenames=nicenames)
	
	actualfit$call<-this.call
	actualfit$formula<-formula
	actualfit$link<-link
	actualfit$pfd<-pfd
	if(!keep.data)
	{
		actualfit$pfd$X<-NULL
	}
	else
	{
		actualfit$data<-data
	}
	actualfit$blocking.variables<-blocking.variables
	class(actualfit)<-"pim"
	#this may not be the cleanest way to do this. Reconsider in the future?
	if("glmnet" %in% class(actualfit$morefitinfo)) class(actualfit)<-c("glmnetpim", class(actualfit))
	return(actualfit)
}