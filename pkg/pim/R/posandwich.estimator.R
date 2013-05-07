#' Pseudo-observation variance sandwich estimator
#' 
#' Pseudo-observation variance sandwich estimator
#' 
#' @aliases posandwich.estimator posandwich.estimator.Uforposandwich Uforposandwich.default Uforposandwich.fakeH0 Uforposandwich-class Uforposandwich variance.estimator
#' 
#' @param U See the formula for sandwich estimator: holds \code{U_{ij}}
#' @param U.diff See the formula for sandwich estimator: holds the partial derivatives of \code{U}.
#' @param g1,g2 Index in the original observations of the "left" and "right" part of the pseudo-
#' 	observations.
#' @param shared.factor Factor by which all \code{UijUik} or \code{UijUlj} will be multiplied
#' @param switched.factor Factor by which all \code{UijUki} or \code{UijUjl} will be multiplied
#' @param self.factor Factor by which all \code{UijUij} or \code{-UijUji} will be multiplied
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return For \code{posandwich.estimator(.Uforposandwich)}: the estimated covariance matrix.
#' @note There should be no reason to use \code{posandwich.estimator} outside of its usage in 
#' 	\code{\link{varianceestimator.sandwich}}...
#' @seealso \code{\link{varianceestimator.sandwich}}
#' @keywords pim covariance sandwich
#' @export
posandwich.estimator<-function(U, U.diff, g1, g2, shared.factor=1, switched.factor=1, self.factor=1, verbosity=0)
{
	#note: this estimator is greatly optimized, based on T Lumley's code!
	fullyequal<-(g1==g2)
	if(any(fullyequal))
	{
		#the contribution of these terms to variance and covariance is zero anyway, because
		#I_{ii} = 0.5 always (ie: constant), so we drop them out!
		U<-U[!fullyequal,,drop=FALSE]
		g1<-g1[!fullyequal]
		g2<-g2[!fullyequal]
	}
	
	usum1.tmp <- rowsum(U,g1,reorder=F)
	usum2.tmp <- rowsum(U,g2,reorder=F)
	usum1  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum1.tmp),0)
	usum2  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum2.tmp),0)
	usum1 [unique(g1),] <- usum1.tmp; usum2[unique(g2),] <- usum2.tmp
	
	utushared<-((t(usum1)%*%usum1)+t(usum2)%*%usum2)
	utuswitched<-((t(usum1)%*%usum2)+t(usum2)%*%usum1)
	uDiag<-t(U)%*%U #Is counted twice as shared.factor, but needs to be counted as self.factor
	if(verbosity>0)
	{
		cat("usums:\n")
		print(cbind(usum1, usum2))
		cat("With colSums:\n")
		print(colSums(cbind(usum1, usum2)))
		cat("utushared:\n")
		print(utushared)
		cat("utuswitched:\n")
		print(utuswitched)
		cat("uDiag:\n")
		print(uDiag)
	}
	
	utu<-shared.factor*utushared  + 
		(switched.factor)*utuswitched +
		(self.factor-2*shared.factor)*(uDiag)
	
	#if the inverses occur (ij, ji), they are counted doubly as switched!!
	#However: they should be counted as - self
	#So I need all of these combinations:
	mx<-nrow(usum1)+1
	#This implementation expects mx not to be too big
	#It should work up to just about sqrt(2 ^ .Machine$double.digits)
	#which is actually above 90.000.000 on my own machine
	uids<-g1*mx+g2
	invuids<-g2*mx+g1
	invrowperrow<-match(uids, invuids)
	rowsWithInv<-(!is.na(invrowperrow)) 
	if(any(rowsWithInv))
	{
		rowsWithInv<-which(rowsWithInv)
		tmp<-sapply(rowsWithInv, function(ri){
			res<-U[ri,] %*% t(U[invrowperrow[ri],]) 
			return(res)
		})
		if(is.null(dim(tmp)))
		{
			if(length(rowsWithInv)==1)
			{
				tmp<-matrix(tmp, ncol=1)
			}
			else
			{
				tmp<-matrix(tmp, nrow=1)
			}
		}
		uijji<-matrix(rowSums(tmp), ncol=ncol(usum1.tmp))
		if(verbosity>0)
		{
			cat("uijji:\n")
			print(uijji)
		}
		
		utu<-utu-(2 *  switched.factor + self.factor)*uijji
	}
	
	#skip this last part if U.diff is identity... (note: NULL here will mean identity)
	if(is.null(U.diff)) return (utu)
	
	U.diff.inv<-solve(U.diff)
	return(U.diff.inv%*%utu%*%U.diff.inv)
}

#' @rdname posandwich.estimator
#' 
#' @param Uforposandwich Result of a function of the form of \code{Uforposandwich.default}
#' 	(note this should be of class \code{"Uforposandwich"})
#' @param poset Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation.
#' @details \code{\link{posandwich.estimator.Uforposandwich}} is essentially a wrapper that
#' 	translates parameters.
#' @export
posandwich.estimator.Uforposandwich<-function(Uforposandwich, poset, verbosity=0)
{
	posandwich.estimator(Uforposandwich$U, Uforposandwich$U.diff, 
											 g1=poset[,1], g2=poset[,2], 
											 shared.factor=Uforposandwich$shared.factor, 
											 switched.factor=Uforposandwich$switched.factor, 
											 self.factor=Uforposandwich$self.factor, 
											 verbosity=verbosity)
}

#' @rdname posandwich.estimator
#' 
#' @param Zbeta Matrix holding \code{Z^T beta}
#' @param Z Design Matrix in the pseudo-observations.
#' @param Y Pseudo-outcomes.
#' @param link Name of the link function.
#' @param W Weights to be applied to the pseudo-observations.
#' @return For \code{Uforposandwich.default}/\code{Uforposandwich.fakeH0}: an object of class 
#' 	\code{"Uforposandwich"}, holding items:
#' \item{U}{see the matching parameter to \code{posandwich.estimator}} 
#' \item{U.diff}{see the matching parameter to \code{posandwich.estimator}} 
#' \item{fv}{Inversely transformed \code{Zbeta}. The predicted probabilistic indicator for each pseudo-observation.} 
#' \item{shared.factor}{see the matching parameter to \code{posandwich.estimator}} 
#' \item{switched.factor}{see the matching parameter to \code{posandwich.estimator}} 
#' \item{self.factor}{see the matching parameter to \code{posandwich.estimator}} 
#' @details \code{\link{Uforposandwich.default}} is the normal function for the sandwich 
#' 	estimator. \code{\link{Uforposandwich.fakeH0}} is provided to generally estimate the
#' 	variance under H0 (with identity link) through the same sandwich estimator code.
#' @export
Uforposandwich.default<-function(Zbeta, Z, Y, link, W=NULL)
{
	if(! is.null(W))
	{
		warning("Currently, weights are not supported in Uforposandwich.default They will be ignored.")
	}
	Zbeta <- c(Zbeta)
	Y <- c(Y)
	if(link == "probit")
	{
		fv <- pnorm(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI) #correction, not mentioned in the article, low impact
		m.d <- dnorm(Zbeta)
		m.dd <- -m.d*Zbeta 
		res <- Y-fv
		U <- Z*m.d*res/var.PI
		#if(!is.null(W)) U<-W * U
		U.diff <- t(Z)%*%(Z*c((var.PI*(m.dd*res - m.d^2) - res*m.d^2*(1-2*fv))/var.PI^2))
	}
	else if(link == "logit")
	{
		fv <- plogis(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI) #correction, not mentioned in the article, low impact
		U <- Z*c(Y-fv)
		#if(!is.null(W)) U<-W * U
		U.diff <- -t(Z)%*%(Z*c(var.PI))
	}
	else if(link == "identity")
	{
		#note: for identity, we leave out the variance. This is OK
		fv<-Zbeta
		U <- Z*c(Y-fv)
		#if(!is.null(W)) U<-W * U
		U.diff <- -t(Z)%*%(Z)
	}
	else
	{
		stop(paste("Unsupported link function for Uforposandwich.default:"), link)
	}
	
	rv<-list(U=U, U.diff=U.diff, fv=fv, shared.factor=1, switched.factor=1, self.factor=1)
	class(rv)<-"Uforposandwich"
	return(rv)
}

#' @rdname posandwich.estimator
#' 
#' @export
Uforposandwich.fakeH0<-function(Zbeta, Z, Y, link, W=NULL)
{
	if(link != "identity")
	{
		stop("You are using Uforposandwich.fakeH0 and thus probably varianceestimator.H0. This is only correct if the link function is the identity.")
	}
	if(! is.null(W))
	{
		warning("Currently, weights are not supported in Uforposandwich.fakeH0. They will be ignored.")
	}
	Zbeta <- c(Zbeta)
	
	tZ<-t(Z)
	fakeU<-t(solve(tZ %*% Z) %*% tZ)
	
	rv<-list(U=fakeU, U.diff=NULL, fv=Zbeta, shared.factor=1/12, switched.factor=-1/12, self.factor=1/4)
	class(rv)<-"Uforposandwich"
	return(rv)
}