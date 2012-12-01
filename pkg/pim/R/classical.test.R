#' PIM implementations of distributionfree tests
#' 
#' PIM implementations of distributionfree tests
#' 
#' @aliases classical.test
#' 
#' @param test Type of "classical" distribution free test to perform.
#' @param data Context where \code{out}, \code{group} and \code{block} are to be interpreted
#' @param out Column of \code{data} that holds the outcomes (responses)
#' @param group Column of \code{data} that holds the predicting variable
#' @param block Column of \code{data} that holds the blocking variable. This can be left out 
#' 	if no blocking is present
#' @param varianceestimator Function like the result of \code{\link{varianceestimator.sandwich}()}
#' 	(the default). The default (\code{\link{varianceestimator.H0}}) will provide the classical
#' 	test statistic. With the Sandwich estimator, you can achieve a Wald-type test.
#' @param alternative As for other tests
#' @param levelP The level of the grouping variable that is the "top" of the umbrella (only
#' 	relevant for \code{test="MackWolfe"}).
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
classical.test<-function(test=c("WilcoxonMannWhitney", "KruskalWallis", "MackSkillings", "BrownHettmansperger", "JonckheereTerpstra", "MackWolfe"), 
												 data, out, group, block, varianceestimator=varianceestimator.H0(), alternative=c("two.sided", "greater", "less"), 
												 levelP,verbosity=0)
{
	test<-match.arg(test)
	alternative<-match.arg(alternative)
	df<-NA
	if(test=="WilcoxonMannWhitney")
	{
		if(!missing(block)) warning("Blocks are ignored in Wilcoxon-Mann-Whitney")
		if(length(levels(as.factor(data[,group]))) != 2) stop("Predictor should have two values for Wilcoxon-Mann-Whitney.")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", poset=pairwiseposet, 
								varianceestimator=varianceestimator, verbosity=verbosity-1, interpretation="regular")
		stat<-(coefficients(pimfit)-0.5)/sqrt(vcov(pimfit)[1,1])
		p.value <- switch(alternative, 
											less = pnorm(stat), 
											greater = pnorm(stat, lower.tail = FALSE), 
											two.sided = 2 * min(pnorm(stat), pnorm(stat, lower.tail = FALSE)))
	}
	else if(test=="KruskalWallis")
	{
		if(!missing(block)) warning("Blocks are ignored in Kruskal-Wallis")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", poset=fullposet, interpretation="marginal", 
								varianceestimator=varianceestimator, verbosity=verbosity-1)
		stat<-t(coefficients(pimfit)-0.5) %*% ginv(vcov(pimfit)) %*% (coefficients(pimfit)-0.5)
		df<-length(levels(as.factor(data[,group])))-1
		p.value <- pchisq(stat, df, lower.tail = FALSE)
	}
	else if(test=="MackSkillings")
	{
		tbl<-table(data[,c(group,block),drop=FALSE])
		nij<-tbl[1,1]
		if(! all(tbl==nij)) stop("Mack-Skillings requires Randomized Complete Block design.")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", blocking.variables=block, poset=fullposet, 
								interpretation="marginal", varianceestimator=varianceestimator, verbosity=verbosity-1)
		stat<-t(coefficients(pimfit)-0.5) %*% ginv(vcov(pimfit)) %*% (coefficients(pimfit)-0.5)
		df<-length(levels(as.factor(data[,group])))-1
		p.value <- pchisq(stat, df, lower.tail = FALSE)
	}
	else if(test=="BrownHettmansperger")
	{
		if(!missing(block)) warning("Blocks are ignored in Brown-Hettmansperger")
		df<-length(levels(as.factor(data[,group])))-1
		if(df!=2) stop("There should be 3 treatments for Brown-Hettmansperger.")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", poset=fullposet, 
								varianceestimator=varianceestimator, verbosity=verbosity-1, 
								interpretation="regular")
		stat<-t(coefficients(pimfit)-0.5) %*% ginv(vcov(pimfit)) %*% (coefficients(pimfit)-0.5)
		pBH<-function(q, mc=1000)
		{
			q<-drop(q)
			v1<-rchisq(mc, df=2)
			v2<-rlogis(mc)
			vals<-v1^2+sqrt(3)/pi * v2^2
			return(mean(q<=vals))
		}
		p.value <- 1-pBH(stat)
	}
	else if(test=="JonckheereTerpstra")
	{
		if(!missing(block)) warning("Blocks are ignored in Jockheere-Terpstra")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", poset=pairwiseposet, 
								varianceestimator=varianceestimator, verbosity=verbosity-1, 
								interpretation="regular")

		ni<-table(data[,group]) #note: I checked, this also contains counts for levels that do not occur, in the order of the levels
		cmb<-combn(length(ni), 2)
		cmbcnt<-ni[cmb[1,]]*ni[cmb[2,]]
		stat<-(cmbcnt %*% (coefficients(pimfit)-0.5)) / sqrt(cmbcnt %*% vcov(pimfit) %*% cmbcnt)
		p.value <- switch(alternative, 
											less = pnorm(stat), 
											greater = pnorm(stat, lower.tail = FALSE), 
											two.sided = 2 * min(pnorm(stat), pnorm(stat, lower.tail = FALSE)))
	}
	else if(test=="MackWolfe")
	{
		if(!missing(block)) warning("Blocks are ignored in Mack-Wolfe")
		if(missing(levelP)) stop("Specify a valid levelP for the top of the umbrella.")
		lvls<-levels(as.factor(data[,group]))
		if(!(levelP %in% lvls)) stop("Specify a valid levelP for the top of the umbrella.")
		
		formula<-stats::formula(paste(out, "~F(", group, ")-1", sep=""))
		pimfit<-pim(formula, data=data, link="identity", poset=pairwiseposet, 
								varianceestimator=varianceestimator, verbosity=verbosity-1, 
								interpretation="regular")
		
		#ni<-sapply(lvls, function(curlvl){sum(x==curlvl)})
		ni<-table(data[,group]) #note: I checked, this also contains counts for levels that do not occur, in the order of the levels
		
		cmb<-combn(length(ni), 2)
		cmbcnt<-ni[cmb[1,]]*ni[cmb[2,]]
		force(cmbcnt)
		
		P<-match(levelP, names(ni))
		K<-(sqrt(8*length(coefficients(pimfit))+1)+1)/2 #just inverted formula
			
		cmbs<-combn(K,2)
		isleft<-(cmbs[1,]<=P) & (cmbs[2,]<=P)
		isright<-(cmbs[1,]>=P) & (cmbs[2,]>=P)
		
		usev<-rep(0, K * (K-1)/2 )
		usev[isleft]<-1
		usev[isright]<- -1
		
		usev<-usev * cmbcnt
		
		enumer<-usev %*%  (coefficients(pimfit)-0.5)
		denom<-sqrt( usev %*% vcov(pimfit) %*% usev)
		
		stat<-enumer/denom

		p.value <- switch(alternative, 
											less = pnorm(stat), 
											greater = pnorm(stat, lower.tail = FALSE), 
											two.sided = 2 * min(pnorm(stat), pnorm(stat, lower.tail = FALSE)))
	}
	else stop(paste("Unknown classical.test:", test) )
	rv<-list(statistic=stat, test=test, p.value=p.value, df=df)
	return(rv)
}