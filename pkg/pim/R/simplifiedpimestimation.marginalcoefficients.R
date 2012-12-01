#' Simplified fitting of PIMs with only one categorical predictor
#' 
#' Simplified fitting of PIMs with only one categorical predictor
#' 
#' @aliases simplifiedpimestimation.marginalcoefficients simplifiedpimestimation.pairwisecoefficients simplifiedpimestimation.marginalcoefficients simplifiedpimestimation.pairwisecovariance
#' 
#' @param data Context where \code{out}, \code{group} and \code{block} are to be interpreted
#' @param out Column of \code{data} that holds the outcomes (responses)
#' @param group Column of \code{data} that holds the predicting variable
#' @param block Column of \code{data} that holds the blocking variable. This can be left out 
#' 	if no blocking is present
#' @return for \code{simplifiedpimestimation.marginalcoefficients}:
#' 	The coefficient estimates of the marginal model, in order of the levels of the predicting variable 
#' @note These functions are merely provided for comparison. The pim function supports
#' 	generic ways of estimating these values.
#' @keywords pim fit simplified
#' @export
simplifiedpimestimation.marginalcoefficients<-function(data, out, group, block)
{
	y<-data[,out]
	x<-as.factor(data[,group])
	b<-as.factor(if(missing(block)) factor(rep(1, nrow(data))) else data[,block])
	sapply(levels(x), function(curx){
		den<-0
		enu<-0
		for(i in seq_along(y))
		{
			ly<-y[i]
			curb<-b[i]
			ry<-y[(b==curb) & (x==curx)]
			den<-den+length(ry)
			enu<-enu+sum((ly<ry)+0.5*(ly==ry))
		}
		enu/den
	})
}

#' @rdname simplifiedpimestimation.marginalcoefficients
#' 
#' @return for \code{simplifiedpimestimation.marginalcovariance}:
#' 	The covariance estimates of the marginal model, in order of the levels of the predicting variable 
#' @export
simplifiedpimestimation.marginalcovariance<-function(data, out, group, block)
{
	y<-data[,out]
	x<-as.factor(data[,group])
	b<-as.factor(if(missing(block)) factor(rep(1, nrow(data))) else data[,block])
	ub<-unique(b)
	nl<-table(x)
	N<-sum(nl)
	
	if(length(ub) == 1)
	{
		#Marginal, no blocks: lemma 3
		rv<-matrix(-(N+1)/(12*N^2), nrow=length(nl), ncol=length(nl))
		diag(rv)<-(N-nl)*(N+1)/(12*N^2*nl)
		colnames(rv)<-rownames(rv)<-names(nl)
	}
	else
	{
		nb<-table(b,x)
		frstn<-as.vector(nb)[1]
		if(all(nb==frstn))
		{
			#RCB
			n<-frstn
			K<-length(nl)
			L<-length(ub)
			rv<-matrix(-(K+1/n)/(12*n*L*K^2), nrow=length(nl), ncol=length(nl))
			diag(rv)<-(K-1)*(K+1/n)/(12*n*L*K^2)
			colnames(rv)<-rownames(rv)<-names(nl)
		}
		else stop("Unsupported case in simplifiedpimestimation.marginalcovariance")
	}
	return(rv)
}

#' @rdname simplifiedpimestimation.marginalcoefficients
#' 
#' @return for \code{simplifiedpimestimation.pairwisecoefficients}:
#' 	The coefficient estimates of the pairwise model, in order of the combined levels of the 
#' 	predicting variable (as \code{combn} generates them)
#' @export
simplifiedpimestimation.pairwisecoefficients<-function(data, out, group, block)
{
	y<-data[,out]
	x<-as.factor(data[,group])
	lvls<-seq_along(levels(x))
	x<-as.integer(x)
	b<-as.factor(if(missing(block)) factor(rep(1, nrow(data))) else data[,block])
	ub<-unique(b)
	lvlcomb<-as.data.frame(t(combn(lvls,2)))
	colnames(lvlcomb)<-c("Lefti", "Rightj")
	lvlcomb$enu<-0
	lvlcomb$den<-0
	for(rnr in seq(nrow(lvlcomb)))
	{
		for(curb in ub)
		{
			ly<-y[x==lvlcomb$Lefti[rnr] & b==curb]
			ry<-y[x==lvlcomb$Rightj[rnr] & b==curb]
			lvlcomb$den[rnr]<-lvlcomb$den[rnr]+length(ly)*length(ry)
			lvlcomb$enu[rnr]<-lvlcomb$enu[rnr]+sum(sapply(ly, function(yi){sum(yi<ry)+0.5*sum(yi==ry)}))
		}
	}
	lvlcomb$beta<-lvlcomb$enu/lvlcomb$den
	return(lvlcomb)
}

#' @rdname simplifiedpimestimation.marginalcoefficients
#' 
#' @return for \code{simplifiedpimestimation.pairwisecovariance}:
#' 	The covariance estimates of the pairwise model, in order of the combined levels of the 
#' 	predicting variable (as \code{combn} generates them)
#' @export
simplifiedpimestimation.pairwisecovariance<-function(data, out, group, block)
{
	y<-data[,out]
	x<-as.factor(data[,group])
	lvls<-seq_along(levels(x))
	x<-as.integer(x)
	b<-as.factor(if(missing(block)) factor(rep(1, nrow(data))) else data[,block])
	ub<-unique(b)
	N<-length(y)
	lvlcomb<-as.data.frame(t(combn(lvls,2)))
	colnames(lvlcomb)<-c("Lefti", "Rightj")
	
	#not very efficient but what the hell
	lvlcomb$nlL<-sapply(lvlcomb$Lefti, function(curx){sum(x==curx)})
	lvlcomb$nlR<-sapply(lvlcomb$Rightj, function(curx){sum(x==curx)})
	
	if(length(ub) == 1)
	{
		#Pairwise, no blocks: lemma 6
		rv<-matrix(0, nrow=nrow(lvlcomb), ncol=nrow(lvlcomb))
		for(i in seq(nrow(rv)))
		{
			for(j in seq(nrow(rv)))
			{
				if(i==j)
				{
					rv[i,j]<- (lvlcomb$nlL[i] + lvlcomb$nlR[i] + 1)/(12 * lvlcomb$nlL[i] * lvlcomb$nlR[i])
				}
				else
				{
					if(lvlcomb$Lefti[i]==lvlcomb$Lefti[j])
					{
						rv[i,j]<- 1/(12 * lvlcomb$nlL[i])
					}
					else if(lvlcomb$Rightj[i]==lvlcomb$Rightj[j])
					{
						rv[i,j]<- 1/(12 * lvlcomb$nlR[j])
					}
					else if(lvlcomb$Lefti[i]==lvlcomb$Rightj[j])
					{
						rv[i,j]<- -1/(12 * lvlcomb$nlL[i])
					}
					else if(lvlcomb$Rightj[i]==lvlcomb$Lefti[j])
					{
						rv[i,j]<- -1/(12 * lvlcomb$nlR[i])
					}
				}
			}
		}
		rownames(rv)<-colnames(rv)<-paste(as.character(lvlcomb$Lefti), as.character(lvlcomb$Rightj), sep=" - ")
	}
	else stop("Unsupported case in simplifiedpimestimation.pairwisecovariance")
	return(rv)
}
