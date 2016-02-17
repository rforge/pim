#' Non-PIM implementations of distributionfree tests
#' 
#' Non-PIM implementations of distributionfree tests
#' 
#' @aliases legacy.WilcoxonMannWhitney legacy.KruskalWallis legacy.MackSkillings legacy.BrownHettmansperger legacy.JonckheereTerpstra legacy.MackWolfe
#' 
#' @param data Context where \code{out}, \code{group} and \code{block} are to be interpreted
#' @param out Column of \code{data} that holds the outcomes (responses)
#' @param group Column of \code{data} that holds the predicting variable
#' @param block Column of \code{data} that holds the blocking variable. This can be left out 
#' 	if no blocking is present
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return a list holding the following items:
#' \item{statistic}{The test statistic.} 
#' \item{p.value}{p-value for the test.} 
#' \item{df}{Degress of freedom (if relevant, otherwise \code{NA}).} 
#' \item{conversion}{Function to convert the pim estimates to the test statistic.} 
#' @note These functions are merely provided for comparison. The pim function supports
#' 	generic ways of estimating these values. For generosity, the parameters to all of
#' 	them are the same, though some are not relevant (e.g. \code{block} for WMW).
#' @keywords pim fit legacy
#' @export
legacy.WilcoxonMannWhitney<-function(data, out, group, block, verbosity=0)
{
	if(!missing(block)) warning("Blocks are ignored in Wilcoxon-Mann-Whitney")
	formula<-as.formula(paste(out, group, sep="~"))
	tst<-wilcox.test(formula, data=data)
	
	x<-as.factor(data[,group])
	n1<-sum(x==levels(x)[1])
	n2<-sum(x==levels(x)[2])
	regstat<-(tst$statistic-n1*n2/2)/(sqrt((n1*n2*(n1+n2+1))/12))
	return(list(
		statistic=regstat, 
		p.value=tst$p.value, 
		df=tst$parameter,
		conversion=function(coef, vcov){(coef-0.5)/sqrt(vcov[1,1])}
		))
}

#' @rdname legacy.WilcoxonMannWhitney
#' 
#' @export
legacy.KruskalWallis<-function(data, out, group, block, verbosity=0)
{
	if(!missing(block)) warning("Blocks are ignored in Kruskal-Wallis")
	formula<-as.formula(paste(out, group, sep="~"))
	tst<-kruskal.test(formula, data=data)
	return(list(
		statistic=tst$statistic, 
		p.value=tst$p.value, 
		df=tst$parameter,
		conversion=function(coef, vcov){t(coef-0.5) %*% ginv(vcov) %*% (coef-0.5)}
		))
}

#' @rdname legacy.WilcoxonMannWhitney
#' 
#' @export
legacy.MackSkillings<-function(data, out, group, block, verbosity=0)
{
	y<-data[,out]
	x<-as.factor(data[,group])
	b<-as.factor(if(is.null(block)) factor(rep(1, nrow(data))) else data[,block])
	
	nij<-sum(x==levels(x)[1] & b == levels(b)[1])
	#RCB
	#Form the matrix like MS.test likes it:
	#columns = blocks
	#rows = treatments + replicates
	MSY<-sapply(levels(b), function(curb){
		as.vector(sapply(levels(x), function(curx){
			y[x==curx & b == curb]
		}))
	})
	
	MSX<-rep(seq_along(levels(x)), each=nij)
	MSreps=nij
	
	#require(asbio)
	#to avoid unnecessary dependency upon asbio, we just reproduce their MS.test function here
	MS.test<-function (Y, X, reps) 
	{
		orderm <- matrix(nrow = nrow(Y), ncol = ncol(Y))
		for (i in 1:ncol(Y)) {
			orderm[, i] <- rank(Y[, i])
		}
		S <- matrix(ncol = 1, nrow = max(X))
		for (i in 1:max(X)) {
			S[i] <- sum(orderm[X == i])/reps
		}
		N <- dim(Y)[1] * dim(Y)[2]
		MS <- (12/(max(X) * (N + ncol(Y))) * sum(S^2)) - 3 * (N + 
			ncol(Y))
		p.val <- pchisq(MS, max(X) - 1, lower.tail = FALSE)
		res <- data.frame(df = max(X) - 1, MS = MS, Chi.sq_p.val = p.val)
		colnames(res) <- c("df", "MS.test.stat", "P(Chi.sq>MS)")
		res
	}
	tst<-MS.test(Y=MSY, X=MSX, reps=MSreps)
	#return(tst)
	return(list(
		statistic=tst$MS.test.stat, 
		p.value=tst$`P(Chi.sq>MS)`, 
		df=tst$df,
		conversion=function(coef, vcov){t(coef-0.5) %*% ginv(vcov) %*% (coef-0.5)}
	))
}

#' @rdname legacy.WilcoxonMannWhitney
#' 
#' @export
legacy.BrownHettmansperger<-function(data, out, group, block, verbosity=0)
{
	if(!missing(block)) warning("Blocks are ignored in Brown-Hettmansperger")
	y<-data[,out]
	x<-as.factor(data[,group])
	lvls<-levels(x)
	if(length(lvls)!=3) warning("There should be 3 treatments for Brown-Hettmansperger")
	
	tForBH<-function(y, x, llvl, rlvl)
	{
		yj<-y[x==rlvl]
		yi<-y[x==llvl]
		dta<-expand.grid(yj, yi)
		#print(head(dta))
		sum(sign(dta$Var1-dta$Var2))
	}
	
	pBH<-function(q, mc=1000)
	{
		v1<-rchisq(mc, df=2)
		v2<-rlogis(mc)
		vals<-v1^2+sqrt(3)/pi * v2^2
		return(mean(q<=vals))
	}
	
	n1<-sum(x==lvls[1])
	n2<-sum(x==lvls[2])
	n3<-sum(x==lvls[3])
	N<-n1+n2+n3
	t12<-tForBH(y, x, lvls[1], lvls[2])
	t23<-tForBH(y, x, lvls[2], lvls[3])
	t31<-tForBH(y, x, lvls[3], lvls[1])
	
	extraterm<-3*n1*n2*n3/N * (t12/(n1*n2) + t23/(n2*n3) +t31/(n3*n1))^2
	
	formula<-as.formula(paste(out, group, sep="~"))
	tst<-kruskal.test(formula, data=data)
	stat<-tst$statistic + extraterm
	
	return(list(
		statistic=stat, 
		p.value=1-pBH(stat), 
		df=2,
		conversion=function(coef, vcov){t(coef-0.5) %*% ginv(vcov) %*% (coef-0.5)}
	))
}

#' @rdname legacy.WilcoxonMannWhitney
#' 
#' @export
legacy.JonckheereTerpstra<-function(data, out, group, block, verbosity=0)
{
	if(!missing(block)) warning("Blocks are ignored in Jockheere-Terpstra")
	
	y<-data[,out]
	x<-as.factor(data[,group])
	lvls<-levels(x)
	
	K<-length(lvls)
	ni<-sapply(lvls, function(curlvl){sum(x==curlvl)})
	N<-sum(ni)
	
	muJT<-(N^2-sum(ni^2))/4
	sigsqJT<-(N^2*(2*N+3) - sum(ni^2*(2*ni+3)))/72
	if(verbosity>0) cat("mu:", muJT, "\n")
	if(verbosity>0)cat("sigsq:", sigsqJT, "\n")
	
	mainterm<-sum(sapply(seq(K-1), function(k){
		lvlk<-lvls[k]
		sum(sapply((k+1):K, function(l){
			lvll<-lvls[l]
			yi<-y[x==lvlk]
			yj<-y[x==lvll]
			IPI(yi, yj)
		}))
	}))
	
	if(verbosity>0)cat("mainterm:", mainterm, "\n")
	
	T.JT<-(mainterm - muJT)/sqrt(sigsqJT)
	#note this gives the same result as the code found e.g. on:
	#https://stat.ethz.ch/pipermail/r-help/2006-June/108442.html
	
	pv<-pnorm(T.JT)
	pv<-2*min(pv, 1-pv)
	
	cmb<-combn(length(ni), 2)
	cmbcnt<-ni[cmb[1,]]*ni[cmb[2,]]
	force(cmbcnt)
	
	return(list(
		statistic=T.JT, 
		p.value=pv, 
		df=NA,
		conversion=function(coef, vcov){
			(cmbcnt %*% (coef-0.5)) / sqrt(cmbcnt %*% vcov %*% cmbcnt)
			}
	)) #note: the normality should be checked - probably not right
}

#' @rdname legacy.WilcoxonMannWhitney
#' 
#' @param levelP The level of the grouping variable that is the "top" of the umbrella.
#' @export
legacy.MackWolfe<-function(data, out, group, block, verbosity=0,levelP)
{
	if(!missing(block)) warning("Blocks are ignored in Mack-Wolfe")
	
	y<-data[,out]
	x<-as.factor(data[,group])
	lvls<-seq_along(levels(x))
	levelP<-match(levelP, levels(x))
	x<-as.integer(x)
	
	K<-length(lvls)
	P<-match(levelP, lvls)
	ni<-sapply(lvls, function(curlvl){sum(x==curlvl)})
	#print(ni)

	N1<-sum(ni[seq(P)])
	N2<-sum(ni[-seq(P-1)])
	N<-sum(ni)
	nP<-ni[P]
	
	muMW<-(N1^2+N2^2-sum(ni^2)-(nP)^2)/4
	sigsqMW<-(2*(N1^3+N2^3)+3*(N1^2+N2^2) - sum(ni^2*(2*ni+3)) - nP^2*(2*nP+3)+12*nP*N1*N2-12*nP^2*(N))/72
	
	leftterm<-sum(sapply(seq(P-1), function(k){
		sum(sapply((k+1):P, function(l){
			yi<-y[x==k]
			yj<-y[x==l]
			tmp<-IPI(yi, yj, res="sumcount")
			return(tmp[1])
		}))
	}))
	
	rightterm<-sum(sapply(P:(K-1), function(k){
		sum(sapply((k+1):K, function(l){
			yi<-y[x==k]
			yj<-y[x==l]
			tmp<-IPI(yj, yi, res="sumcount")
			return(tmp[1])
		}))
	}))
	
	if(verbosity>0) cat("mu:", muMW, "\n")
	if(verbosity>0)cat("sigsq:", sigsqMW, "\n")
	if(verbosity>0)cat("leftterm:", leftterm, "\n")
	if(verbosity>0)cat("rightterm:", rightterm, "\n")
	
	T.MW<-(leftterm + rightterm - muMW)/sqrt(sigsqMW)
	
	pv<-pnorm(T.MW)
	pv<-2*min(pv, 1-pv)

	cmb<-combn(length(ni), 2)
	cmbcnt<-ni[cmb[1,]]*ni[cmb[2,]]
	force(cmbcnt)
	
	force(levelP)
	return(list(
		statistic=T.MW, 
		p.value=pv, 
		df=NA,
		conversion=function(coef, vcov){
			P<-levelP
			K<-(sqrt(8*length(coef)+1)+1)/2 #just inverted formula
			
			cmbs<-combn(K,2)
			isleft<-(cmbs[1,]<=P) & (cmbs[2,]<=P)
			isright<-(cmbs[1,]>=P) & (cmbs[2,]>=P)
			
			usev<-rep(0, K * (K-1)/2 )
			usev[isleft]<-1
			usev[isright]<- -1
			
			usev<-usev * cmbcnt
			
			enumer<-usev %*%  (coef-0.5)
			denom<-sqrt( usev %*% vcov %*% usev)
			
			enumer/denom
		}
	)) #note: the normality should be checked - probably not right
}
