#' Actually fit the pim
#' 
#' Actually fit the pim from prepared data
#' 
#' @aliases pim.fit
#' 
#' @param pfd Object of class \code{\link{pimfitdata}}
#' @param link Name of the link function (defaults to \code{"identity"})
#' @param estimator Function like the result of \code{\link{estimator.nleqslv}()} (the 
#' 	default). See there to find the required form of this function or some provided 
#' 	alternatives.
#' @param varianceestimator Function like the result of \code{\link{varianceestimator.sandwich}()}
#' 	(the default). See there to find the required form of this function or some 
#' 	provided alternatives.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param nicenames Defaults to \code{TRUE}: try to make the column names more readable.
#' @return List with (some of) the items:
#' \item{coefficients}{Parameter estimates. See \code{\link{estimator.nleqslv}}} 
#' \item{morefitinfo }{Parameter estimation info. See \code{\link{estimator.nleqslv}}} 
#' \item{fitted.values}{Predicted pseudo-observation values.} 
#' \item{vcov }{Covariance estimates. See \code{\link{varianceestimator.sandwich}}} 
#' \item{morevarfitinfo }{Covariance estimation info. See \code{\link{varianceestimator.sandwich}}}
#' @details The presence or absence or form of the items in the return value depends mainly upon
#' 	the passed along \code{estimator} and \code{varianceestimator}.
#' 	
#' 	If \code{estimator} fails, no attempt is taken to estimate the covariances (and the matching
#' 	items are not in the return value). If \code{varianceestimator} was \code{NULL}, the items
#' 	are simply set to \code{NULL}.
#' 	
#' 	If \code{varianceestimator} is executed, but it fails, the error is saved in an additional 
#' 	item \code{varestimationerror}.
#' @note Typically, you will not use this method directly (similarly as \code{\link{glm.fit}} 
#' 	for \code{\link{glm}})
#' @seealso \code{\link{estimator.nleqslv}} \code{\link{varianceestimator.sandwich}}
#' @keywords pim fit
#' @examples set.seed(1)
#' 	iris$out<-factor(sample(2, nrow(iris), replace=TRUE))
#' 	poset<-fullposet(iris, out~Sepal.Length)$poset
#' 	pfd<-pim.fit.prep(out~Sepal.Length, data=iris, poset=poset)
#' 	pf<-pim.fit(pfd)
#' @export
pim.fit<-function(pfd, link=c("logit", "identity", "probit", "inverse", "1/mu^2", "log"), estimator=estimator.nleqslv(), 
									varianceestimator=varianceestimator.sandwich(), verbosity=0, nicenames=TRUE)
{
	link<-match.arg(link)
	if(verbosity>0) cat("Actual coefficient estimation\n")
	estimationresult<-try(estimator(startvalues=NULL, pfd=pfd, link=link))
	if(inherits(estimationresult, "try-error")) return(estimationresult)
	
	cn<-gsub(" ", "", pfd$pimformula$names, fixed=TRUE)
	#However: handle the special case where intercept has been added to
	#the set of coefficients:
	if(is.null(dim(estimationresult$coefficients)))
	{
		colnamesafterfit<-names(estimationresult$coefficients)
	}
	else
	{
		colnamesafterfit<-rownames(estimationresult$coefficients)
	}
	numfinalcoefs<-length(colnamesafterfit)
	if(numfinalcoefs > length(cn))
	{
		if(("(Intercept)" %in% colnamesafterfit) && (! ("(Intercept)" %in% cn) ))
		{
			if(length(cn) + 1 == length(colnamesafterfit))
			{
				if(verbosity > 0)
				{
					cat("Intercept was added by the fitting procedure. Will attempt to handle it in the same spot.\n")
					cat("Names were:\n")
					print(cn)
				}
				ipos<-match("(Intercept)", colnamesafterfit)
				if(ipos==1) lft<-NULL else lft<-cn[seq(ipos-1)]
				if(ipos==length(colnamesafterfit)) rght<-NULL else rght<-cn[(ipos):(length(cn))]
				cn<-c(lft, "(Intercept)", rght)
				if(verbosity > 0)
				{
					cat("Names are now:\n")
					print(cn)
				}
			}
			else
			{
				warning("Intercept and more was added by the fitting procedure. Something probably went wrong there...")
			}
		}
		else
		{
			warning("Unexpected columns added by the fitting procedure. Something probably went wrong there...")
		}
	}
	if(nicenames)
	{
		if(verbosity > 0)
		{
			cat("Will replace to nicer names in column names:", cn, "\n")
			cat("\tReplace:", pfd$pimformula$full.colnames, "\n")
			cat("\tWith:", pfd$pimformula$nice.colnames, "\n")
		}
		for(i in seq_along(pfd$pimformula$full.colnames)){
			cn<-gsub(pfd$pimformula$full.colnames[i], pfd$pimformula$nice.colnames[i], cn, fixed=TRUE)
		}
	}
	
	if(is.null(dim(estimationresult$coefficients)))
	{
		names(estimationresult$coefficients)<-cn
	}
	else
	{
		rownames(estimationresult$coefficients)<-cn
	}
	retval<-estimationresult
	retval$fitted.values<-.predict(beta=estimationresult$coefficients, Z=pfd$X, link=link)
	if(!is.null(varianceestimator))
	{
		if(verbosity>0) cat("(Co)variance estimation\n")
		varestimationresult<-try(varianceestimator(estimationresult, pfd, link=link, estimator=estimator, verbosity=verbosity-1))
		if(inherits(varestimationresult, "try-error"))
		{
			retval$varestimationerror<-varestimationresult
		}
		else
		{
			retval<-c(retval, varestimationresult)
		}
	}
	else
	{
		retval$vcov<-NULL
		retval$morevarfitinfo<-"None"
	}
	return(retval)
}
