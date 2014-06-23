#' Generate sample data that is appropriate for the classical distributionfree tests
#' 
#' Generate sample data that is appropriate for the classical distributionfree tests
#' 
#' @aliases demo.WilcoxonMannWhitney demo.KruskalWallis demo.MackSkillings demo.JonckheereTerpstra demo.MackWolfe demo.BrownHettmansperger
#' 
#' @param n Number of observations
#' @param groupmeans Means of normal outcome distribution for each group (note this implies the number of groups)
#' @param force.balanced If \code{TRUE}, the design is forced to be balanced (an error occurs if this is impossible)
#' @return a list holding the following items:
#' \item{dta}{A \code{data.frame} holding the generated data. The outcome is column \code{"y"}, 
#' 	the predictor is \code{"x"} and the blocking variable (if relevant) is \code{"b"}} 
#' \item{n}{As passed in.} 
#' \item{groupmeans}{As passed in.} 
#' For \code{demo.MackSkillings} these additional items are present:
#' \item{blocks}{Number of blocks.} 
#' \item{blockeffects}{(Random) effect of each specific block.} 
#' \item{treatments}{Number of groups in the predicting variable.} 
#' \item{tb1}{\code{data.frame} holding all combinations of groups and blocks.} 
#' \item{tbmult}{Similar as \code{tb1}, but now holding the repetitions and also randomized.} 
#' @keywords pim data legacy
#' @export
demo.WilcoxonMannWhitney<-function(n=100, groupmeans=c(0,1), force.balanced=FALSE)
{
	#Wilcoxon-Mann-Whitney data
	if(length(groupmeans) != 2) stop("Wilcoxon-Mann-Whitney expects two groups.")
	demo.JonckheereTerpstra(n=n, groupmeans=groupmeans, force.balanced=force.balanced)
}

#' @rdname demo.WilcoxonMannWhitney
#' 
#' @export
demo.KruskalWallis<-function(n=100, groupmeans=c(0,1,2), force.balanced=FALSE)
{
	#Kruskal-Wallis data
	demo.JonckheereTerpstra(n=n, groupmeans=groupmeans, force.balanced=force.balanced)
}

#' @rdname demo.WilcoxonMannWhitney
#' 
#' @param blockeffects Effect of each block (note this implies the number of blocks)
#' @param replications Number of replications per group/block combination
#' @export
demo.MackSkillings<-function(blockeffects=rnorm(4), groupmeans=c(0,1,2), replications=10, force.balanced=FALSE)
{
	#Mack-Skillings data
	mssdta<-list(blocks=length(blockeffects))
	mssdta$blockeffects<-blockeffects
	mssdta$treatments<-length(groupmeans)
	mssdta$groupmeans<-groupmeans
	mssdta$n<-mssdta$blocks*mssdta$treatments*replications
	mssdta$tb1<-as.matrix(expand.grid(seq(mssdta$treatments), seq(mssdta$blocks)))
	mssdta$tbmult<-mssdta$tb1[rep(seq(nrow(mssdta$tb1)), replications),][sample(mssdta$n),] #reorder them just for fun?
	colnames(mssdta$tbmult)<-c("x", "b")
	table(as.data.frame(mssdta$tbmult))
	mssdta$dta<-data.frame(y=rnorm(mssdta$n), mssdta$tbmult)
	mssdta$dta$y<-mssdta$dta$y + mssdta$groupmeans[mssdta$dta$x] + mssdta$blockeffects[mssdta$dta$b]
	mssdta$dta$x<-as.ordered(mssdta$dta$x)
	mssdta$dta$b<-as.ordered(mssdta$dta$b)
	
	return(mssdta)
}

#' @rdname demo.WilcoxonMannWhitney
#' 
#' @export
demo.JonckheereTerpstra<-function(n=100, groupmeans=c(0,1,2,4), force.balanced=FALSE)
{
	#Jonckheere-Terpstra data
	if(force.balanced)
	{
		if(n %% length(groupmeans) != 0) stop("Number of observations is not a multiple of number of groups so balanced is impossible")
		wmwdta2<-data.frame(y=rnorm(n), x=ordered(sample(rep(seq_along(groupmeans), length.out=n))))
	}
	else
	{
		wmwdta2<-data.frame(y=rnorm(n), x=ordered(sample(length(groupmeans), n, replace=TRUE)))
	}
	for(i in seq_along(groupmeans))
	{
		wmwdta2$y[wmwdta2$x==i]<-wmwdta2$y[wmwdta2$x==i] + groupmeans[i]
	}
	list(dta=wmwdta2, n=n, groupmeans=groupmeans)
}

#' @rdname demo.WilcoxonMannWhitney
#' 
#' @export
demo.MackWolfe<-function(n=100, groupmeans=c(0,1,5,2), force.balanced=FALSE)
{
	#Mack-Wolfe data
	demo.JonckheereTerpstra(n=n, groupmeans=groupmeans, force.balanced=force.balanced)
}

#' @rdname demo.WilcoxonMannWhitney
#' 
#' @export
demo.BrownHettmansperger<-function(n=100, groupmeans=c(0,1,2), force.balanced=FALSE)
{
	#Brown-Hettmansperger data
	demo.KruskalWallis(n=n, groupmeans=groupmeans, force.balanced=force.balanced)
}
