#' Check the symmetry condition for a PIM
#' 
#' Check the symmetry condition for a PIM
#' 
#' @aliases pimsym
#' 
#' @param formula Original formula
#' @param data Context where the formula \code{formula} is to be interpreted
#' @param link Name of the link function (defaults to \code{"logit"})
#' @param blocking.variables Character vector holding column names that hold blocking
#' 	variables.
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
#' @param lhs \code{"PO"}, \code{"<"} or \code{"<="}: Unequality used for the lefthandside of 
#' 	the formula. The default (\code{"PO"}) is the normal probabilistic index.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param interactions.difference If \code{TRUE} (note that the default is  
#' 	\code{interpretation!="marginal"}) interaction terms will be interpreted as the  
#' 	differences of the onesided interaction terms (if this is possible at all). This is  
#' 	unsupported if \code{unsupported if} is \code{"marginal"}. Some special interaction 
#' 	terms with calculated columns may lead to unexpected behaviour.
#' @param threshold When checking the symmetry condition, how much digression is allowed.
#' @return \code{TRUE} if the symmetry condition appears to be fulfilled.
#' @details This check is implemented by taking a random set of coefficients (from 
#' uniform distributions over \code{]-0.5,0.5[}) and comparing the predicted
#' probabilities based on them. So: not a formal check, though the error should
#' be ignorable.
#' 
#' Be aware that for sizeable datasets, this check may take quite some time and
#' (temporarily) use a lot of memory.
#' 
#' Note that the same check is performed by \code{\link{pim.fit.prep}} (and thus by 
#' \code{\link{pim}}) when \code{check.symmetric=TRUE}.
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
pimsym<-function(formula, data, link=c("logit", "identity", "probit", "inverse", "1/mu^2", "log"), 
							blocking.variables=character(),
							leftsuffix="_L", rightsuffix="_R", 
							interpretation=c("difference", "regular", "marginal", "symmetric"), na.action=na.fail,
							lhs=c("PO", "<", "<="), verbosity=0, 
							interactions.difference=(interpretation!="marginal"), threshold=1e-6)
{
	interpretation<-match.arg(interpretation)
	if(interpretation %in% c("difference", "symmetric")) return(TRUE)
	link<-match.arg(link)
	poset<-.symposet(data, formula, verbosity=verbosity-1)$poset
	#Note: I rely on the specific order implied by .symposet in what follows!
	#That is: if there are 2N rows in it, then row i and N + i are each other's "inverse"
	#Note: this will still be the case after applying blocks!
	#Note: the "self"-pseudo-observations occur twice in this poset, because they
	#represent their own "inverse"
	
	if(verbosity>0) cat("Converted formula\n")
	data<-na.action(data)
	formulaconv<-pimformula(formula=formula, data=data, interpretation=interpretation, verbosity=verbosity-1, leftsuffix=leftsuffix, 
													rightsuffix=rightsuffix, lhs=lhs, interactions.difference=interactions.difference)
	
	poset<-.filter.poset.blockingvariables(data, poset, blocking.variables)

	if(verbosity>0) cat("Get design matrix\n")
	qpd<-.quickpimdata(pimform=formulaconv, data=data, poset=poset, na.action=na.action, nicenames=FALSE, verbosity=verbosity-1, makenames=.symrownames)
	
	if(verbosity>0) cat("Check symmetry condition\n")
	issym<-.quicksimcheck(qpd$X, link, threshold=threshold)
	rm(qpd, poset)
	
	return(issym)
}