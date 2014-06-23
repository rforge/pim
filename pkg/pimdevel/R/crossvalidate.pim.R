#' Crossvalidate a glmnet PIM
#' 
#' Crossvalidate a glmnet PIM
#' 
#' @aliases crossvalidate.pim cv.pim-class cv.pim
#' 
#' @param pimob Object of class \code{\link{pim}}. Note: it should really be of class 
#' 	\code{\link{glmnetpim}} in the current implementation.
#' @param method \code{"fullsplit"} splits pseudo-observations over folds, \code{"semisplit"} takes
#' 	the nonvalidating pseudo-observations together for the fit for each fold, \code{"naive"} simply
#' 	crossvalidates over the pseudo-observations (note: typically, this is not correct!)
#' @param type.measure,nfolds,foldid,weights See \code{cv.glmnet}
#' @param \dots Passed on to \code{cv.glmnet} (or \code{.cvpo.glmnet}).
#' @param include.extrainfo If \code{TRUE} (not the default), the return value contains two extra 
#' 	items, \code{foldid} and \code{foldFits}, that may be used to reassess the crossvalidation value 
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return An object of class \code{cv.pim}, of class \code{cv.glmnet}, and depending on the type
#' 	of linkfunction (see \code{cv.glmnet}) some more classes.
#' It holds all the items of the original \code{pimob} and all necessary items for a 
#' 	\code{cv.glmnet} object (so the relevant S3 methods like \code{print} and \code{plot} will
#' 	work for them.)
#' @seealso \code{\link{pim}} \code{cv.glmnet}
#' @keywords pim crossvalidate
#' @examples set.seed(1)
#' pen.N<-100
#' pen.noisep<-50
#' pen.noisemat<-matrix(rnorm(pen.N*pen.noisep), nrow=pen.noisep)
#' pendta<-data.frame(y=rnorm(pen.N), x=factor(sample(2, pen.N, replace=TRUE)), pen.noisemat)
#' pendta$y[pendta$x=="2"]<-pendta$y[pendta$x=="2"]+1
#' colnames(pendta)[(1:pen.noisep)+2]<-paste("X", 1:pen.noisep, "X", sep="")
#' pen.formula<-paste("y~", paste(c("F(x)", setdiff(colnames(pendta), c("x", "y"))), collapse="+"), sep="")
#' 
#' penpim<-pim(as.formula(pen.formula), data=pendta, link="identity", poset=noselfposet, 
#' 	estimator=estimator.glmnet(), varianceestimator=NULL, keep.data=TRUE, verbosity=0,
#' 	interpretation="regular")
#' cv.penpim.naive<-crossvalidate.pim(penpim, method="naive")
#' cv.penpim<-crossvalidate.pim(penpim, method="fullsplit")
#' cv.penpim.ps<-crossvalidate.pim(penpim, method="semisplit")
#' @export
crossvalidate.pim<-function(pimob, method=c("fullsplit", "semisplit", "naive"), type.measure=c("mse", "deviance", "class", "auc", "mae"), 
														nfolds = 10, foldid, weights,..., include.extrainfo=FALSE, verbosity=0)
{
	if(!inherits(pimob$morefitinfo, "glmnet")) stop("crossvalidate.pim is currently only supported for glmnet-fitted pims.")
	method<-match.arg(method)
	
	X<-pimob$pfd$X
	Y<-pimob$pfd$Y
	
	if(method=="naive")
	{
		#crossvalidate over the pseudo-observations
		if(missing(weights))
		{
			#If we find Y-values equal to 0.5 (indicating ties), we handle this by weighted fitting
			istie<-Y==0.5
			if(any(istie))
			{
				warning("Ties found in crossvalidate.pim. Applying weighted design matrix reconstruction.")
				ties<-1+istie
				tiereps<-rep(seq_along(ties), ties)
				X<-X[tiereps,,drop=FALSE] #repeat the rows with ties twice
				weights<-1/ties[tiereps] #weight those doubled observations by a half
				multiplyby<-do.call(c,lapply(istie, function(curtie){if(!curtie) 1 else c(0,2)}))
				Y<-Y[tiereps] * multiplyby
			}
			else
			{
				weights<-rep(1, length(Y))
			}
		}
		cvfit<-cv.glmnet(x=X, y=Y, weights=weights, offset=pimob$morefitinfo$usedoffset, lambda=pimob$morefitinfo$lambda, 
										 type.measure=type.measure, standardize=pimob$morefitinfo$standardize, alpha=pimob$morefitinfo$usedalpha,
										 nfolds = nfolds, foldid=foldid, family=pimob$morefitinfo$usedfamily,...)
	}
	else
	{
		#crossvalidate over the original observations
		cvfit<-.cvpo.glmnet(x=X, y=Y, weights=weights, poset=pimob$pfd$poset, offset=pimob$morefitinfo$usedoffset, lambda=pimob$morefitinfo$lambda, 
												type.measure=type.measure, standardize=pimob$morefitinfo$standardize, alpha=pimob$morefitinfo$usedalpha,
												nfolds = nfolds, family=pimob$morefitinfo$usedfamily,..., fullsplit=(method=="fullsplit"), verbosity=verbosity)
	}
	cvfit$glmnet.fit<-pimob$morefitinfo
	rv<-c(pimob, cvfit)
	rv$method<-method
	class(rv)<-c("cv.pim", class(cvfit))
	return(rv)
}