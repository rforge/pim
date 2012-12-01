#' Probabilistic Index / Indicator for sets of variables
#' 
#' Probabilistic Index / Indicator for sets of variables
#' 
#' @aliases IPI
#' 
#' @param yi,yj Variables for which the I/PI will be calculated. \code{yi} will always be
#' 	at the left hand side of the unequality.
#' @param method If \code{"pairwise"} (the default), then all combinations of items in \code{yi}
#' 	and \code{yj} are used. Otherwise both are taken in the order specified (implying equal lengths)
#' @param res Calculate the \code{"sum"} (default), \code{"mean"} (i.e. probabilistic index)
#' 	of the combinations, or simply return \code{"all"} partial results. Finally, \code{"sumcount"}
#' 	returns both the sum and the count.
#' @return If \code{res} is \code{"sum"} or \code{"mean"}, a single value, otherwise a
#' 	\code{data.frame} with the follwing columns
#' \item{yi}{Left hand side of the combination.} 
#' \item{yj}{Right hand side of the combination.} 
#' \item{pi}{The probabilistic indicator for this combination.} 
#' @keywords pim probabilistic index indicator
#' @export
IPI<-function(yi,yj, method=c("pairwise", "paired"), res=c("sum", "mean", "all", "sumcount"))
{
	method<-match.arg(method)
	res<-match.arg(res)
	if(method=="paired")
	{
		if(length(yi) != length(yj)) stop("Cannot compare paired when lengths are not the same")
		comps<-data.frame(i=seq(length(yi)),j=seq(length(yi)))
	}
	else
	{
		comps<-expand.grid(seq(length(yi)), seq(length(yj)))
		colnames(comps)<-c("i", "j")
	}
	yi<-yi[comps$i]
	yj<-yj[comps$j]
	
	pri<-(yi<yj) + 0.5*(yi==yj)
	
	if(res=="sum") return(sum(pri))
	if(res=="mean") return(mean(pri))
	if(res=="sumcount") return(c(sum(pri), length(pri)))
	
	return(data.frame(yi=yi, yj=yj, pi=pri))
}
