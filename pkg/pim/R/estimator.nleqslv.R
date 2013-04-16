#' Estimators for the PIM parameters
#' 
#' Estimators for the PIM parameters
#' 
#' @aliases estimator.nleqslv estimator estimator.glm estimator.glmnet estimator.BB scorefunctioncreator.default estimator.lqa scorefunctioncreator
#' 
#' @param jac,global,xscalm See \code{\link{nleqslv}}.
#' @param method See \code{\link{nleqslv}} / \code{BBsolve} / \code{lqa}.
#' @param control See \code{\link{nleqslv}} / \code{BBsolve} / \code{\link{glm.fit}} 
#' 	/ \code{lqa}.
#' @param scoreFunctionCreator Function that will create the score vector function. This
#' 	defaults to \code{scorefunctioncreator.default} and should be of the same form (and 
#' 	return a function of the same form as \code{scorefunctioncreator.default}).
#' @param treat.convergence.error Defaults to \code{"warn"}, so a warning will be issued with 
#' 	the errorcode of the \code{\link{nleqslv}} or \code{BBsolve} call. \code{"error"} 
#' 	will generate an error on convergence issues. \code{"log"} will simply note the occurence
#' 	in the output window and \code{"ignore"} will do just that. In most situations you can 
#' 	avoid the convergence issues by properly specifying the nleqslv parameters. This can be 
#' 	investigated e.g. through \code{estimator.trymultiple}.
#' @return These functions (\code{estimator.*}) each return a function themselves. The returned
#' 	function should have three parameters (\code{startvalues}, a set of initial estimates of the 
#' 	parameters; \code{pfd}, an object of class \code{\link{pimfitdata}} ; \code{link}, the name 
#' 	of the link function) and should itself return a list of two items:
#' \item{coefficients}{The parameter estimates} 
#' \item{morefitinfo }{Implementation specific information on the fit}
#' @details These functions estimate the coefficients (/parameters) of the PIM by solving the
#' 	(sometimes) nonlinear equations. Each calls upon another library for the solution and some
#' 	may be more accurate / correct / performant depending on the specific model.
#' 	
#' 	\code{estimator.glm} returns the regular \code{\link{glm}} estimate (assuming the pseudo-
#' 	observations are independent)
#' 	
#' 	\code{estimator.glmnet} returns the elsatic net penalized \code{glmnet} estimate 
#' 	(assuming, again, the pseudo-observations are independent)
#' 	
#' 	For the different implementations, \code{morefitinfo} contains:
#' 	\enumerate{
#' \item \code{estimator.nleqslv} The return value of the \code{\link{nleqslv}} call
#' \item \code{estimator.glm} The return value of the \code{\link{glm}} call
#' \item{estimator.glmnet} The return value of the \code{glmnet} call, with some added 
#' 	items: \code{usedalpha}, \code{usedfamily},\code{usedoffset} and \code{standardize} 
#' \item \code{estimator.BB} The return value of the \code{BBsolve} call 
#' }
#' @note For \code{estimator.glmnet}, \code{coefficients} contains a sparse \code{\link{matrix}} 
#' 	holding the coefficient estimates and intercept (!) for all lambda values.
#' @keywords pim estimate coefficient
#' @export
estimator.nleqslv<-function(jac = NULL, method = c("Broyden", "Newton"), 
														global = c("dbldog", "pwldog", "qline", "gline", "none"), 
														xscalm = c("fixed", "auto"), control = list(ftol=1e-6),
														scoreFunctionCreator=scorefunctioncreator.default,
														treat.convergence.error=c("warn", "error", "log", "ignore"))
{
	treat.convergence.error<-match.arg(treat.convergence.error)
	force(jac)
	force(method)
	force(global)
	force(xscalm)
	force(control)
	force(scoreFunctionCreator)
	force(treat.convergence.error)
	rv<-function(startvalues=NULL, pfd, link)	{
		fn<-scoreFunctionCreator(pfd$X,pfd$Y,link)
		if(is.null(startvalues)) startvalues<-rep(0, ncol(pfd$X))
		thefit<-nleqslv(startvalues, fn, jac = jac, method = method, global = global, xscalm = xscalm, control = control)
		fit.x <- thefit$x
		if(thefit$termcd != 1)
		{
			.handleError(paste("Fit could not be obtained: nonconvergence of the algorithm:", thefit$message), treat.convergence.error)
		}
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
	attr(rv, "reqs")<-"nleqslv"
	return(rv)
}
attr(estimator.nleqslv, "reqs")<-"nleqslv"
#' @rdname estimator.nleqslv
#' 
#' @export
estimator.glm<-function(control=list())
{
	force(control) #so that it will be available in the function we return
	actualfunction<-function(startvalues=NULL, pfd, link)
	{
		family<-.glmfamily(link)
		
		dta<-.handleSpecialData(intercept.handling=FALSE, yties.handling=TRUE,  pfd=pfd)
		
		thefit<-glm.fit(x=dta$X, y=dta$Y, start=startvalues, family=family, weights=dta$wts,
										control=control, intercept=pfd$intercept)
		fit.x<-thefit$coefficients
		class(thefit)<-unique(c(class(thefit), "glm"))
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
}

#' @rdname estimator.nleqslv
#' 
#' @param alpha,nlambda,lambda,standardize See \code{glmnet}.
#' @param penalize.intercepts If an intercept is present in the model, penalize it or not.
#' @export
estimator.glmnet<-function(alpha=1, nlambda = 100, lambda=NULL, standardize=TRUE, penalize.intercepts=FALSE)
{
	force(alpha)
	force(nlambda)
	force(lambda)
	force(standardize)
	force(penalize.intercepts)
	actualfunction<-function(startvalues=NULL, pfd, link)
	{
		#Trick to map link functions back to acceptable families...
		families<-list(identity="gaussian", logit="binomial", probit="unsupported", inverse="unsupported", `1/mu^2`="unsupported", log="poisson")
		family<-families[[link]]
		if(family=="unsupported") stop("family is unsupported for estimator.glmnet")
		
		#note: we will only ever include an unpenalized intercept in the model if
		#such a column was present in the design matrix, this was confirmed by
		#pfd$intercept _AND_ penalize.intercepts was FALSE.
		#This is perfectly equivalent to having dta$itcind be NA or not.
		dta<-.handleSpecialData(intercept.handling=!penalize.intercepts, yties.handling=TRUE,  pfd=pfd)
		unpenalized.intercept=is.na(dta$itcind)
		
		thefit<-glmnet(x=dta$X, y=dta$Y, family=family, weights=dta$wts, alpha=alpha, nlambda=nlambda, 
									 lambda=lambda, standardize=standardize, intercept=unpenalized.intercept)
		thefit$usedalpha=alpha
		thefit$usedfamily=family
		thefit$usedoffset=NULL
		thefit$standardize=standardize
		
		bta<-.restoreIntercept(beta=thefit$beta, itc=thefit$a0, itcind=dta$itcind)
		
		return(list(coefficients=bta, morefitinfo=thefit) )
	}
	attr(actualfunction, "reqs")<-"glmnet"
	return(actualfunction)
}
attr(estimator.glmnet, "reqs")<-"glmnet"

#' @rdname estimator.nleqslv
#' 
#' @param quiet See \code{BBsolve}.
#' @export
estimator.BB<-function(method=c(2,3,1), control=list(), quiet=FALSE,
											 scoreFunctionCreator=scorefunctioncreator.default,
											 treat.convergence.error=c("warn", "error", "log", "ignore"))
{
	treat.convergence.error<-match.arg(treat.convergence.error)
	force(method)
	force(control)
	force(quiet)
	force(scoreFunctionCreator)
	force(treat.convergence.error)
	rv<-function(startvalues=NULL, pfd, link)	{
		fn<-scoreFunctionCreator(pfd$X,pfd$Y,link)
		if(is.null(startvalues)) startvalues<-rep(0, ncol(pfd$X))
		thefit<-BBsolve(startvalues, fn, method = method, control = control, quiet = quiet )
		fit.x <- thefit$par
		if(thefit$convergence != 0)
		{
			.handleError(paste("Fit could not be obtained: nonconvergence of the algorithm:", thefit$message), treat.convergence.error)
		}
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
	attr(rv, "reqs")<-"BB"
	return(rv)
}
attr(estimator.BB, "reqs")<-"BB"

#' @rdname estimator.nleqslv
#' 
#' @details \code{estimator.trymultiple} is a special case that will simply try to use
#' 	\code{nleqslv}, \code{} and \code{} with several parameters, hoping that one may
#' 	lead to a fit. Although it provides the best chance of getting a fit, take care in
#' 	assuring that the fit matches the covariance estimator! Also be aware that this may
#' 	be slow.
#' @export
estimator.trymultiple<-function(scoreFunctionCreator=scorefunctioncreator.default)
{
	nleqslvargs<-expand.grid(method=c("Broyden", "Newton"), 
													 global=c("dbldog", "pwldog", "qline", "gline", "none"), 
													 xscalm=c("fixed","auto"), stringsAsFactors=FALSE)
	tryfunctions<-c(lapply(seq(nrow(nleqslvargs)), function(i){
		list(fn=estimator.nleqslv, method=nleqslvargs$method[i], 
				 global=nleqslvargs$global[i], 
				 xscalm=nleqslvargs$xscalm[i], 
				 scoreFunctionCreator=scoreFunctionCreator)
	}), list(fn=estimator.BB, scoreFunctionCreator=scoreFunctionCreator), list(fn=estimator.glm))
	errs<-list()
	force(tryfunctions)
	force(errs)
	rv<-function(startvalues=NULL, pfd, link)	{
		for(fn in tryfunctions)
		{
			actualfn<-fn$fn
			fn$fn<-NULL
			estfn<-do.call(actualfn, fn)
			rv<-try(estfn(startvalues=startvalues, pfd=pfd, link=link), silent=TRUE)
			if(!inherits(rv, "try-error"))
			{#successful fit!
				cat("The first set of parameters that did not result in convergence issues, was:\n")
				print(fn)
				return(rv)
			}
			errs<-c(errs, rv)
		}
		stop("None of the attempts resulted in proper estimation. Error messages were:\n\n", paste(errs, sep="\n*\n"))
	}
	attr(rv, "reqs")<-c("nleqslv", "BB")
	return(rv)
}
attr(estimator.trymultiple, "reqs")<-c("nleqslv", "BB")

#' @rdname estimator.nleqslv
#' 
#' @param Z Pseudo-observation design matrix
#' @param Y Pseudo-observations.
#' @param link Name of the link function.
#' @return For \code{scorefunctioncreator.default}: a function that takes a set of parameter 
#' 	estimates (\code{beta}) and calculates a set of values that should be solved for zero.
#' @export
scorefunctioncreator.default<-function(Z,Y,link)
{
	if (link == "probit") {
		U.func <- function(beta) {
			Zbeta <- c(Z %*% beta)
			colSums(Z * dnorm(Zbeta) * c(Y - pnorm(Zbeta))/c(pnorm(Zbeta) * (1 - pnorm(Zbeta))))
		}
	}
	else if (link == "logit"){
		U.func <- function(beta) {
			Zbeta <- c(Z %*% beta)
			colSums(Z * c(Y - plogis(Zbeta)))
		}
	}
	else if (link == "identity") #have to check this, but it should be right
	{
		U.func <- function(beta) {
			Zbeta <- as.vector(c(Z %*% beta))
			colSums(Z * c(Y - Zbeta))
		}
	}
	else
	{
		stop(paste("Unsupported link function for scorefunctioncreator.default:"), link)
	}
	return(U.func)
}

#' @rdname estimator.nleqslv
#' 
#' @param penalty Any \code{lqa}-supported penalty.
#' @export
estimator.lqa<-function(control=lqa.control(), penalty = NULL, method = "lqa.update2", standardize = TRUE, penalize.intercepts=FALSE)
{
	force(control) #so that it will be available in the function we return
	force(penalty) #so that it will be available in the function we return
	force(method) #so that it will be available in the function we return
	force(standardize) #so that it will be available in the function we return
	force(penalize.intercepts) #so that it will be available in the function we return
	actualfunction<-function(startvalues=NULL, pfd, link)
	{
		family<-.glmfamily(link=link)
		
		dta<-.handleSpecialData(intercept.handling=!penalize.intercepts, yties.handling=TRUE,  pfd=pfd)
		unpenalized.intercept=is.na(dta$itcind)

		thefit<-lqa(x=dta$X, y=dta$Y, start=startvalues, family=family, control=control, intercept=unpenalized.intercept,
								penalty = penalty, method = method, standardize = standardize, weights=dta$wts)
		
		thefit$standardize=standardize
		
		#bta<-.restoreIntercept(beta=thefit$beta, itc=thefit$a0, itcind=dta$itcind)
		bta<-thefit$coef #note: if intercept was needed, it will already be in here
		
		return(list(coefficients=bta, morefitinfo=thefit) )
	}
	attr(actualfunction, "reqs")<-"lqa"
	return(actualfunction)
}
attr(estimator.lqa, "reqs")<-"lqa"
