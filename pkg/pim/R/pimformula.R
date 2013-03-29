#' Convert formula to pim formula
#' 
#' Convert formula to pim formula (incorporating L/R and poset)
#' 
#' @aliases pimformula pim.fit.prep pimformula-class pimfitdata-class pimfitdata
#' 
#' @param formula Original formula
#' @param data Context where the formula \code{formula} is to be interpreted
#' @param interpretation If \code{"marginal"} (not the default) parts of the formula are 
#' 	converted to imply marginal pim modeling (see e.g. \code{\link{Mainreplacetext}}). If
#' 	it is \code{"difference"}, then the design matrix of the PIM is the difference of the
#' 	design matrices of each part of the pseudo-observations. The default option is
#' 	\code{"regular"}, which will interpret unaltered columns as differences. A new option
#' 	is \code{"symmetric"}, which works the same as \code{"regular"}, but will enforce
#' 	the symmetry condition by making the sign switch when changing the order (typically,
#' 	this is achieved by subtracting the inverse for each dummy).
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param leftsuffix,rightsuffix Suffixes that will be added to the 'left' and 'right'
#' 	observation's column name in the pseudo-observation. Note: no checking is done that 
#' 	these suffixes are safe, so the wrong suffixes may lead to unexpected behaviour.
#' @param extra.variables Character vector of column names you want to force present in the
#' 	pseudo-observations
#' @param lhs \code{"PO"}, \code{"<"} or \code{"<="}: Unequality used for the lefthandside of 
#' 	the formula. The default (\code{"PO"}) is the normal probabilistic index.
#' @param rhsreplacers List of functions (see \code{\link{Lreplacetext}} and others) that 
#' 	will be used to process the right hand side of the formula. Each function should have 
#' 	the same signature as \code{\link{Lreplacetext}}.
#' @param lhsreplacer Function like \code{\link{LHSreplacetext}} that will be used to reformat 
#' 	the left hand side of the formula
#' @param interactions.difference If \code{TRUE} (note that the default is  
#' 	\code{interpretation!="marginal"}) interaction terms will be interpreted as the  
#' 	differences of the onesided interaction terms (if this is possible at all). This is  
#' 	unsupported if \code{unsupported if} is \code{"marginal"}. Some special interaction 
#' 	terms with calculated columns may lead to unexpected behaviour.
#' @param extra.nicenames Should be a \code{data.frame} containing two character columns: 
#' 	\code{org} and \code{nice}. For each "constructed" column name, provide a nicer one,  
#' 	that will make the results more readable. You may also use parts of constructed column 
#' 	names. Note: make sure to use \code{stringsAsFactor=FALSE} when creating the \code{data.frame}.
#' @return For \code{pimformula}: an object of class "pimformula". The items in this object are:
#' \item{newformula}{The formula containing all suffixed variable names} 
#' \item{left.variables }{\code{\link{data.frame}} containing one row for each variable 
#' 		pertained in the "left" observations, and two columns: \code{org} and \code{fixed},
#' 		containing the original name and the suffixed name of each variable.} 
#' \item{right.variables }{\code{\link{data.frame}} containing one row for each variable 
#' 		pertained in the "right" observations, and two columns: \code{org} and \code{fixed},
#' 		containing the original name and the suffixed name of each variable.} 
#' \item{names }{Character vector holding the names for each individual term in the right
#' 	hand side of the formula. Note: currently this is in no way cleaned up!}
#' \item{full.colnames }{Character vector holding the constructed parts in the formula. Should 
#' 	have the same length as \code{nice.colnames}}
#' \item{nice.colnames }{Character vector holding nicer names the constructed parts in the formula.
#' 	Should have the same length as \code{full.colnames}}
#' @details Main function, doing the actual work.
#' The idea is to convert the formula to text and replace 4(+) kind of "spiced" variables:
#'	O(var) gets replaced with I(var_R<var_L) (see \code{\link{Oreplacetext}} for exact formulation)
#' F(var) gets replaced with Sum I(var_R=i)I(var_L=j) (see \code{\link{Freplacetext}} for exact formulation)
#' L(var) gets replaced with var_L
#' R(var) gets replaced with var_R
#' var not in any of the above cases gets replaced by either var_R-varL (interpretation!="marginal")
#' or by var_L (interpretation=="marginal")
#' 
#' Some sanity checks are already performed, but not all of them (I guess)
#' @seealso \code{\link{Lreplacetext}}
#' @keywords pim formula
#' @examples set.seed(1)
#' iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
#' iris$xord<-as.ordered(iris$Species)
#' pimformula(out~Sepal.Length, data=iris)
#' pimformula(out~I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) ), data=iris, interpretation="regular")
#' pimformula(out~O(xord), data=iris, interpretation="regular")
#' pimformula(out~F(Species), data=iris, interpretation="regular")
#' @export
pimformula<-function(formula, data, interpretation=c("difference", "regular", "marginal", "symmetric"), verbosity=0, 
										 leftsuffix="_L", rightsuffix="_R", extra.variables=character(), lhs=c("PO", "<", "<="),
										 rhsreplacers=list(F=Freplacetext, O=Oreplacetext, L=Lreplacetext, R=Rreplacetext), lhsreplacer=LHSreplacetext, 
										 interactions.difference=(interpretation!="marginal"), extra.nicenames=data.frame(org=character(), nice=character(), stringsAsFactors=FALSE))
{
	interpretation<-match.arg(interpretation)
	interactions.difference<-interactions.difference[1]
	if(interactions.difference & (interpretation=="marginal") ) stop("Cannot make interactions differences for marginal models.")
	
	if(interpretation=="difference")
	{
		return(.pimformula.difference(formula=formula, data=data, verbosity=verbosity, leftsuffix=leftsuffix, 
																	rightsuffix=rightsuffix, extra.variables=extra.variables, 
																	lhs=lhs,lhsreplacer=lhsreplacer))
	}
	
	cf<-as.character(formula)
	
	response<-cf[2]
	response<-gsub("[[:space:]]", "",response) #remove all whitespace
	rhs<-gsub("[[:space:]]", "",cf[3]) #remove all whitespace
	
	maybevars<-colnames(data)
	if(verbosity > 0) cat("Colnames:", maybevars, "\n")
	
	#First, do a sanity check on the columns:
	lvars<-paste(maybevars, leftsuffix, sep="")
	if(any(lvars %in% maybevars)) stop("The left suffix cause contradicting column names. This is not supported.")
	rvars<-paste(maybevars, rightsuffix, sep="")
	if(any(rvars %in% maybevars)) stop("The right suffix cause contradicting column names. This is not supported.")
	
	#If some column names are part of others, we handle the 'bigger' ones first
	maybevars<-.safereordercolnames(maybevars)
	
	tmp<-.find.variables(rhs, maybevars, verbosity=verbosity)
	regexvars<-tmp$regexvars
	varsfound<-tmp$varsfound
	
	orgtxts<-gsub("[[:space:]]", "",as.character(extra.nicenames$org))
	nicetxts<-as.character(extra.nicenames$nice)
	
	if(interactions.difference)
	{
		formula<-.diff.interactions(formula=formula, data=data, leftsuffix=leftsuffix, rightsuffix=rightsuffix, nondifferablefunctions=names(rhsreplacers), verbosity=verbosity-1)
		orgtxts<-c(orgtxts, formula$orglabels)
		nicetxts<-c(nicetxts, formula$nicelabels)
		formula<-formula$formula
		cf<-as.character(formula)
		if(verbosity > 0)
		{
			cat("Formula parts after differing interactions:\n")
			print(cf)
		}
		rhs<-gsub("[[:space:]]", "",cf[3]) #remove all whitespace
	}

	#The orglabels may still contain L() and R(), so:
	for(curvari in which(varsfound["any",]))
	{
		curvar<-maybevars[curvari]
		#cat("orgtxts before lrep:\n", orgtxts, "\n")
		lrt<-Lreplacetext(curvar, data=data, verbosity=0, leftsuffix=leftsuffix, rightsuffix=rightsuffix, interpretation="regular")[1:2]
		orgtxts<-gsub(lrt[1], lrt[2], orgtxts, fixed=TRUE)
		#cat("orgtxts after lrep:\n", orgtxts, "\n")
		rrt<-Rreplacetext(curvar, data=data, verbosity=0, leftsuffix=leftsuffix, rightsuffix=rightsuffix, interpretation="regular")[1:2]
		orgtxts<-gsub(rrt[1], rrt[2], orgtxts, fixed=TRUE)
		#cat("orgtxts after rrep:\n", orgtxts, "\n")
	}
	
	newrhs<-rhs
	for(curvari in which(varsfound["any",]))
	{
		curvar<-maybevars[curvari]
		if(verbosity>0) cat( "Acting on variable", curvar, "\n")
		#functions L, R, O and F
		replstmp<-lapply(rhsreplacers, function(curf){
			curf(curvar, data=data, verbosity=verbosity-1, leftsuffix=leftsuffix, rightsuffix=rightsuffix, interpretation=interpretation)
		})
		
		replacements<-sapply(replstmp, "[", 1:2)
		for(cn in seq(ncol(replacements)))
		{
			if(verbosity >1)
			{
				cat("Will replace", replacements[1,cn], "with", replacements[2,cn], "\n")
			}
			newrhs<-gsub(replacements[1,cn], replacements[2,cn], newrhs, fixed=TRUE)
		}
		#replace X by X_R - X_L
		mrt<-Mainreplacetext(curvar, data=data, verbosity=verbosity-1, leftsuffix=leftsuffix, rightsuffix=rightsuffix, interpretation=interpretation)
		if(verbosity >1)
		{
			cat("Will replace", mrt[1], "with", mrt[2], "\n")
		}
		newrhs<-gsub(mrt[1], mrt[2], newrhs, perl=TRUE)
		
		mrtlbls<-mrt[2+seq((length(mrt)/2)-1)]
		mrtnice<-mrt[-seq((length(mrt)/2)+1)]
		
		orgtxts<-c(orgtxts, do.call(c, lapply(replstmp, function(txts){txts[2+seq((length(txts)/2)-1)]})),mrtlbls)
		nicetxts<-c(nicetxts,do.call(c, lapply(replstmp, function(txts){txts[length(txts)/2 + 1 +seq((length(txts)/2)-1)]})), mrtnice)
		
		if(interpretation=="marginal")
		{
			if(grepl(paste(regexvars[curvari], leftsuffix, sep=""), newrhs))
			{
				stop(paste("Could not force marginality for variable", curvar))
			}
		}
	}
	
	#In the response, similar to O-text, but it requires variants, so a separate function
	if(verbosity >1) cat("Response was:", response, "\n")
	ort<-lhsreplacer(response, data=data, verbosity=0, leftsuffix=leftsuffix, rightsuffix=rightsuffix, lhs=lhs)
	repfrom<-response
	if(verbosity >1)
	{
		cat("Will replace (response)", ort[1], "with", ort[2], "in", repfrom , "\n")
	}
	newresponse<-gsub(ort[1], ort[2], repfrom, fixed=TRUE)
	orgtxts<-gsub(" ", "", c(orgtxts, ort[3]), fixed=TRUE)
	nicetxts<-c(nicetxts, ort[4])
	
	left.variables<-unique(c(maybevars[varsfound["includeL",]], response, extra.variables))
	left.variables<-data.frame(org=left.variables, fixed=left.variables, stringsAsFactors=FALSE)
	if(nrow(left.variables) > 0) left.variables$fixed<-paste(left.variables$fixed, leftsuffix, sep="")
	right.variables<-unique(c(maybevars[varsfound["includeR",]], response, extra.variables))
	right.variables<-data.frame(org=right.variables, fixed=right.variables, stringsAsFactors=FALSE)
	if(nrow(right.variables) > 0) right.variables$fixed<-paste(right.variables$fixed, rightsuffix, sep="")
	
	if(verbosity>1)
	{
		cat("Will now try to interpret the following as the resulting formula:\n")
		cat("\"", paste(newresponse, newrhs, sep="~"), "\"\n")
	}
	newformula<-as.formula(paste(newresponse, newrhs, sep="~"))

	#need to check this, but the use of this variable may be obsolete.
	#Look below in pim.fit.prep, where it's overwritten...
	nms<-attr(terms(newformula), "term.labels")
	if(attr(terms(newformula), "intercept") > 0) nms<-c("(Intercept)", nms)
	retval<-list(newformula=newformula, left.variables=left.variables, right.variables=right.variables, 
							 names=nms, full.colnames=orgtxts, nice.colnames=nicetxts, orgresp=response) 
	
	class(retval)<-"pimformula"
	
	return(retval)
}

#' @rdname pimformula
#' 
#' @param blocking.variables Character vector holding column names that hold blocking
#' 	variables.
#' @param poset Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation.
#' @param na.action Defaults to \code{\link{na.fail}}: handles missing data in \code{data}.
#' @param nicenames Defaults to \code{TRUE}: try to make the column names more readable.
#' @param check.symmetric Defaults to \code{TRUE}: if the model does not support the
#'  symmetry condition, a warning is displayed.
#' @param link,threshold See \code{\link{pim}}: only needed to check the symmetry condition.
#' @return For \code{pim.fit.prep}: an object of class "pimfitdata". The items in this object are:
#' \item{X}{The design matrix in pseudo-observation space} 
#' \item{Y }{The pseudo-observations} 
#' \item{poset }{Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation. Note: in some cases this
#' 	is not the passed in \code{poset}, eg when blocks were present.}
#' \item{intercept }{Holds \code{TRUE} if the formula contains an intercept.} 
#' \item{pimformula }{Result of \code{pimformula} function.}
#' \item{original.colnames }{If \code{nicenames} was \code{TRUE}, this will hold the column names
#' 	before "nicing up".}
#' @note TODO: Should probably disallow using intercept in some cases
#' Also have to consider whether passing in contrasts is relevant/possible
#' @seealso \code{\link{Lreplacetext}}
#' @keywords pim formula
#' @examples set.seed(1)
#' iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
#' iris$xord<-as.ordered(iris$Species)
#' pim.fit.prep(out~Sepal.Length, data=iris)
#' pim.fit.prep(out~I((R(Sepal.Length) - L(Sepal.Length))/sqrt(R(Sepal.Length) * L(Sepal.Length)) ), data=iris, interpretation="regular")
#' pim.fit.prep(out~O(xord), data=iris, interpretation="regular")
#' pim.fit.prep(out~F(Species), data=iris, interpretation="regular")
#' @export
pim.fit.prep<-function(formula, data, blocking.variables=character(), poset=t(combn(nrow(data),2)), leftsuffix="_L", rightsuffix="_R", 
											 interpretation=c("difference", "regular", "marginal", "symmetric"), na.action=na.fail, lhs=c("PO", "<", "<="), 
											 verbosity=0,  nicenames=TRUE, interactions.difference=(interpretation!="marginal"), 
											 extra.nicenames=data.frame(org=character(), nice=character(), stringsAsFactors=FALSE),
											 check.symmetric=TRUE, link, threshold=1e-6)
{
	interpretation<-match.arg(interpretation)
	interactions.difference<-interactions.difference[1]
	data<-na.action(data)
	formulaconv<-pimformula(formula=formula, data=data, interpretation=interpretation, verbosity=verbosity-1, leftsuffix=leftsuffix, 
											rightsuffix=rightsuffix, lhs=lhs, interactions.difference=interactions.difference, extra.nicenames=extra.nicenames)
	
	poset<-.filter.poset.blockingvariables(data, poset, blocking.variables)
	
	if(interpretation=="difference")
	{
		dd<-.LRDiffData(data=data, poset=poset, resp=formulaconv$orgresp, formula=formulaconv$newformula, suffixes=c(leftsuffix, rightsuffix))
		
		retval<-list(Y=dd$Y, X=dd$X, poset=poset, intercept=FALSE, pimformula=formulaconv, original.colnames=colnames(dd$X))
		class(retval)<-"pimfitdata"
		return(retval)
		
	}
	
	if(check.symmetric)
	{
		if(missing(link))
		{
			warning("Could not check symmetry condition because the link was not passed into pim.fit.prep.")
		}
		else
		{
			if(!(interpretation %in% c("difference", "symmetric")))
			{
				cposet<-.symposet(data, formula, verbosity=verbosity-1)$poset
				#Note: I rely on the specific order implied by .symposet in what follows!
				#That is: if there are 2N rows in it, then row i and N + i are each other's "inverse"
				#Note: this will still be the case after applying blocks!
				#Note: the "self"-pseudo-observations occur twice in this poset, because they
				#represent their own "inverse"
				
				qpd<-.quickpimdata(pimform=formulaconv, data=data, poset=cposet, na.action=na.action, nicenames=FALSE, verbosity=verbosity-1, makenames=.symrownames)
				
				issym<-.quicksimcheck(qpd$X, link, threshold=threshold)
				if(! issym) warning("The PIM model does not fulfill the symmetry condition, so will be valid at most for a subset of the pseudo-observations.")
				rm(qpd, cposet)
			}
		}
	}
	
	retval<-.quickpimdata(pimform=formulaconv, data=data, poset=poset, na.action=na.action, nicenames=nicenames, verbosity=verbosity-1)
	return(retval)
}
