#' Estimators for the PIM parameters
#' 
#' Estimators for the PIM parameters
#' 
#' @aliases estimator.nleqslv estimator.glm estimator.glmnet estimator.BB scorefunctioncreator.default
#' 
#' @param jac,global,xscalm See \code{\link{nleqslv}}.
#' @param method See \code{\link{nleqslv}} / \code{\link{BBsolve}}.
#' @param control See \code{\link{nleqslv}} / \code{\link{BBsolve}} / \code{\link{glm.fit}}.
#' @param scoreFunctionCreator Function that will create the score vector function. This
#' 	defaults to \code{scorefunctioncreator.default} and should be of the same form (and 
#' 	return a function of the same form as \code{scorefunctioncreator.default}).
#' @param ignore.error Defaults to \code{FALSE}, but if \code{TRUE}, the errorcode of the 
#' 	\code{\link{nleqslv}} or \code{\link{BBsolve}} call will not be checked. Use at your own risk.
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
#' 	\code{estimator.glmnet} returns the elsatic net penalized \code{\link{glmnet}} estimate 
#' 	(assuming, again, the pseudo-observations are independent)
#' 	
#' 	For the different implementations, \code{morefitinfo} contains:
#' 	\enumerate{
#' \item \code{estimator.nleqslv} The return value of the \code{\link{nleqslv}} call
#' \item \code{estimator.glm} The return value of the \code{\link{glm}} call
#' \item{estimator.glmnet} The return value of the \code{\link{glmnet}} call, with some added 
#' 	items: \code{usedalpha}, \code{usedfamily},\code{usedoffset} and \code{standardize} 
#' \item \code{estimator.BB} The return value of the \code{\link{BBsolve}} call 
#' }
#' @note For \code{estimator.glmnet}, \code{coefficients} contains a sparse \code{\link{matrix}} 
#' 	holding the coefficient estimates and intercept (!) for all lambda values.
#' @keywords pim estimate coefficient
#' @export
estimator.nleqslv<-function(jac = NULL, method = c("Broyden", "Newton"), 
														global = c("dbldog", "pwldog", "qline", "gline", "none"), 
														xscalm = c("fixed", "auto"), control = list(),
														scoreFunctionCreator=scorefunctioncreator.default,
														ignore.error=FALSE)
{
	force(jac)
	force(method)
	force(global)
	force(xscalm)
	force(control)
	force(scoreFunctionCreator)
	force(ignore.error)
	rv<-function(startvalues=NULL, pfd, link)	{
		fn<-scoreFunctionCreator(pfd$X,pfd$Y,link)
		if(is.null(startvalues)) startvalues<-rep(0, ncol(pfd$X))
		thefit<-nleqslv(startvalues, fn, jac = jac, method = method, global = global, xscalm = xscalm, control = control)
		fit.x <- thefit$x
		if((!ignore.error) & (thefit$termcd != 1))
		{
			stop(paste("Fit could not be obtained: nonconvergence of the algorithm:", thefit$message))
		}
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
	return(rv)
}

#' @rdname estimator.nleqslv
#' 
#' @export
estimator.glm<-function(control=list())
{
	force(control) #so that it will be available in the function we return
	actualfunction<-function(startvalues=NULL, pfd, link)
	{
		#Trick to map link functions back to acceptable families...
		families<-list(identity="gaussian", logit="binomial", probit="binomial", inverse="Gamma", `1/mu^2`="inverse.gaussian", log="poisson")
		family<-families[[link]]
		family <- get(family, mode = "function", envir = parent.frame())
		if (is.function(family)) family <- family()
		if (is.null(family$family)) {
			print(family)
			stop("'family' not recognized")
		}
		
		thefit<-glm.fit(x=pfd$X, y=pfd$Y, start=startvalues, family=family, control=control, intercept=pfd$intercept)
		fit.x<-thefit$coefficients
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
}

#' @rdname estimator.nleqslv
#' 
#' @param alpha,nlambda,lambda,standardize See \code{\link{glmnet}}.
#' @export
estimator.glmnet<-function(alpha=1, nlambda = 100, lambda=NULL, standardize=TRUE)
{
	force(alpha)
	force(nlambda)
	force(lambda)
	force(standardize)
	actualfunction<-function(startvalues=NULL, pfd, link)
	{
		#Trick to map link functions back to acceptable families...
		families<-list(identity="gaussian", logit="binomial", probit="unsupported", inverse="unsupported", `1/mu^2`="unsupported", log="poisson")
		family<-families[[link]]
		if(family=="unsupported") stop("family is unsupported for estimator.glmnet")
		
		if(!pfd$intercept) stop("For now: not supported not to include intercept in glmnet fitting.")
		itcind<-match("(Intercept)", colnames(pfd$X))
		if(is.na(itcind)) stop("Something went wrong finding the intercept column in glmnet fitting.")
		X<-pfd$X[,-itcind, drop=FALSE]
		
		thefit<-glmnet(x=X, y=pfd$Y, family=family, alpha=alpha, nlambda=nlambda, lambda=lambda, standardize=standardize)
		thefit$usedalpha=alpha
		thefit$usedfamily=family
		thefit$usedoffset=NULL
		thefit$standardize=standardize
		beta<-rbind2(thefit$a0, thefit$beta)
		rownames(beta)<-c("(Intercept)", rownames(thefit$beta))
		return(list(coefficients=beta, morefitinfo=thefit) )
	}
}

#' @rdname estimator.nleqslv
#' 
#' @param quiet See \code{\link{BBsolve}}.
#' @export
estimator.BB<-function(method=c(2,3,1), control=list(), quiet=FALSE,
											 scoreFunctionCreator=scorefunctioncreator.default,
											 ignore.error=FALSE)
{
	force(method)
	force(control)
	force(quiet)
	force(scoreFunctionCreator)
	force(ignore.error)
	rv<-function(startvalues=NULL, pfd, link)	{
		fn<-scoreFunctionCreator(pfd$X,pfd$Y,link)
		if(is.null(startvalues)) startvalues<-rep(0, ncol(pfd$X))
		thefit<-BBsolve(startvalues, fn, method = method, control = control, quiet = quiet )
		fit.x <- thefit$par
		if((!ignore.error) & (thefit$convergence != 0))
		{
			stop(paste("Fit could not be obtained: nonconvergence of the algorithm:", thefit$message))
		}
		return(list(coefficients=fit.x, morefitinfo=thefit) )
	}
	return(rv)
}

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
				return(rv)
			}
			errs<-c(errs, rv)
		}
		stop("None of the attempts resulted in proper estimation. Error messages were:\n\n", paste(errs, sep="\n*\n"))
	}
	return(rv)
}

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
# 			if(all(beta==0))
# 			{
# 				cat("First try:\nZ=\n")
# 				print(Z)
# 				cat("Y=\n")
# 				print(Y)
# 			}
			Zbeta <- c(Z %*% beta)
			colSums(Z * dnorm(Zbeta) * c(Y - pnorm(Zbeta))/c(pnorm(Zbeta) * (1 - pnorm(Zbeta))))
		}
	}
	else if (link == "logit"){
		U.func <- function(beta) {
# 			if(all(beta==0))
# 			{
# 				cat("First try:\nZ=\n")
# 				print(Z)
# 				cat("Y=\n")
# 				print(Y)
# 			}
			Zbeta <- c(Z %*% beta)
			colSums(Z * c(Y - plogis(Zbeta)))
		}
	}
	else if (link == "identity") #have to check this, but it should be right
	{
		U.func <- function(beta) {
# 			if(all(beta==0))
# 			{
# 				cat("First try:\nZ=\n")
# 				print(Z)
# 				cat("Y=\n")
# 				print(Y)
# 			}
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