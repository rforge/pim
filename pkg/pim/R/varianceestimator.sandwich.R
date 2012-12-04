#' Estimators for the PIM parameter variances
#' 
#' Estimators for the PIM parameter variances
#' 
#' @aliases varianceestimator.sandwich varianceestimator.H0 varianceestimator.glm varianceestimator.bootstrap
#' 
#' @param Uforposandwich function like \code{\link{Uforposandwich.default}} (the default) to
#' 	calculate the necessary parts for \code{\link{posandwich.estimator}}.
#' @return These functions (\code{varianceestimator.*}) each return a function themselves. The returned
#' 	function should have five parameters (\code{estimationresult}, the parameter estimates themselves 
#' 	(result of \code{estimation.*}); \code{pfd}, an object of class \code{\link{pimfitdata}} ; \code{link}, the name 
#' 	of the link function; \code{estimator}, the estimator used to generate \code{estimationresult}; \code{verbosity}, see elsewhere) and should itself return a list of two items:
#' \item{vcov}{The estimated covariance matrix} 
#' \item{morevarfitinfo }{Implementation specific information on the variance estimation}
#' @details These functions estimate the variances of the parameter estimates of the PIM. 
#' 	Each provides a solution under different circumstances and some
#' 	may be more accurate / correct / performant depending on the specific model.
#' 	
#' 	For the different implementations, \code{morevarfitinfo} contains:
#' 	\enumerate{
#' \item \code{varianceestimator.sandwich}The return value of the \code{\link{Uforposandwich}} call. This 
#' 	estimate is the generally valid sandwich estimator for the (co)variance. 
#' \item \code{varianceestimator.H0}The return value of the \code{\link{Uforposandwich}} call. This 
#' 	estimate is the estimator for the (co)variance under H0. 
#' \item \code{varianceestimator.glm}The return value of the \code{\link{glm}} call. This estimate is
#' 	only valid if the pseudo-observations are somehow independent and the linkfunction is identity. 
#' \item \code{varianceestimator.bootstrap}A matrix holding the coefficient estimates over all bootstrap
#' 	repetitions. The columns represent the repetitions 
#' 	}
#' @keywords pim estimate covariance
#' @export
varianceestimator.sandwich<-function(Uforposandwich=Uforposandwich.default)
{
	force(Uforposandwich) #so that it will be available in the function we return
	actualfunction<-function(estimationresult, pfd, link, estimator, verbosity=0)
	{
		SV.tmp <- Uforposandwich(pfd$X%*%estimationresult$coefficients, pfd$X, pfd$Y, link) 
		varcov <-   posandwich.estimator.Uforposandwich(SV.tmp, pfd$poset, verbosity=verbosity)
		rownames(varcov) <- colnames(varcov) <- names(estimationresult$coefficients)
		return(list(vcov=varcov, morevarfitinfo=SV.tmp))
	}
	return(actualfunction)
}

#' @rdname varianceestimator.sandwich
#' 
#' @export
varianceestimator.H0<-function(Uforposandwich=Uforposandwich.fakeH0)
{
	varianceestimator.sandwich(Uforposandwich)
}

#' @rdname varianceestimator.sandwich
#' 
#' @param control See \code{\link{glm.fit}}.
#' @export
varianceestimator.glm<-function(control=list())
{
	force(control) #so that it will be available in the function we return
	actualfunction<-function(estimationresult, pfd, link, estimator, verbosity=0)
	{
		if(! inherits(estimationresult$morefitinfo, "glm"))
		{
			warning("Using glm (independence) covariance estimates while the coefficient estimates are not the glm estimates.")
			usefunction<-estimator.glm(control)
			estimationresult<-usefunction(startvalues=estimationresult$coefficients, pfd=pfd, link=link)
		}
		return(list(vcov=vcov(estimationresult$morefitinfo), morevarfitinfo=estimationresult$morefitinfo))
	}
	return(actualfunction)
}

#' @rdname varianceestimator.sandwich
#' 
#' @param D Number of bootstrap repetitions (defaults to 500).
#' @param keep.posetbs If this is \code{TRUE} (not the default), the \code{morevarfitinfo}
#' 	will be a list of two items: \code{allcoefs} containing the fitted coefficients, and
#' 	\code{posetbs}, containing which pseudoobservations were bootstrapped. In both, the 
#' 	bootstrap repetitions are in the columns.
#' @export
varianceestimator.bootstrap<-function(D=500, keep.posetbs=FALSE)
{
	force(D) #so that it will be available in the function we return
	force(keep.posetbs) #so that it will be available in the function we return
	actualfunction<-function(estimationresult, pfd, link, estimator, verbosity=0)
	{
		#browser()
		allcoefs<-lapply(seq(D), function(i){
			curpfd<-.basicbootstrap(pfd, verbosity=verbosity-1)
			curest<-try(estimator(startvalues=NULL, pfd=curpfd, link=link), silent=TRUE)
			if(inherits(curest, "try-error"))
			{
				#in some of the bootstraps, the model may become unstable.
				#For now, we just consider this as NA data and hope most of 
				#them are indeed solvable
				return(list(curcoefficients=rep(NA, length(estimationresult$coefficients)), selectedpo=curpfd$selectedpo) )
			}
			curcoefficients<-curest$coefficients
			names(curcoefficients)<-curpfd$pimformula$names
			return(list(curcoefficients=curcoefficients, selectedpo=curpfd$selectedpo))
		})
		if(keep.posetbs)
		{
			posetbs<-lapply(allcoefs, "[[", "selectedpo")
		}
		allcoefs<-t(vapply(allcoefs, "[[", estimationresult$coefficients, "curcoefficients"))
		if(any(is.na(allcoefs)))
		{
			warning("In varianceestimator.bootstrap, some of the bootstrap repetitions were unstable and thus left out.")
		}
		if((dim(allcoefs)[1]==1) & (D > 1))
		{
			#result of vapply was a vector, turned into the wrong matrix by t, so transpose again
			allcoefs<-t(allcoefs)
		}
		colnames(allcoefs)<-names(estimationresult$coefficients)
		vc<-cov(allcoefs, use="complete.obs")
		if(keep.posetbs) allcoefs<-list(allcoefs=allcoefs, posetbs=posetbs)
		return(list(vcov=vc, morevarfitinfo=allcoefs))
	}
	return(actualfunction)
}

