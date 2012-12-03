#' Get a set of pseudo-observation observation indices
#' 
#' Get a set of pseudo-observation observation indices
#' 
#' @aliases poset fullposet pairwiseposet onewayposet forcedcolorderonewayposet oldpimposet oldpimposetbft
#' 
#' @param data Context where the formula \code{formula} is to be interpreted
#' @param formula Original formula
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return List with 2 items:
#' \item{data}{Similar to the passed along \code{data}, but may contain fewer rows.} 
#' \item{poset }{Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation.} 
#' @details The provided implementations differ as follows:
#' \enumerate{
#' \item \code{fullposet} Contains all combinations of rowindices.
#' \item \code{pairwiseposet} Check that predicting variables can be ordered, and select 
#' 	only combinations where the predictors are bigger on the right side. 
#' \item \code{onewayposet} Similar to \code{pairwiseposet}, but simply uses row index.
#' \item \code{forcedcolorderonewayposet} First reorders the data based on a set of given 
#' 	column names. Note: the other functions here are to be passed along as 
#' 	\code{poset=fullposet}, this one needs \code{poset=forcedcolorderonewayposet(c("col1", "col2"))}
#' \item \code{oldpimposet} and \code{oldpimposetbft} Sorts the data according to all 
#' 	predictors, then does oneway-style. These are mainly provided for comparison to a
#' 	previous implementations of pim.
#' }
#' @seealso \code{\link{pim}}
#' @keywords pim fit poset
#' @examples set.seed(1)
#' 	dta<-data.frame(y=runif(6), x=ordered(sample(3, 6, replace=TRUE)))
#' 	fullposet(dta, y~x)$poset
#' 	pairwiseposet(dta, y~x)$poset
#' 	onewayposet(dta, y~x)$poset
#' @export
fullposet<-function(data, formula, verbosity=0)
{
	n<-nrow(data)
	
	org<-seq(n)
	poset<-cbind(rep(org, n), rep(org, each=n))
	return(list(data=data, poset=poset))
}

#' @rdname fullposet
#' 
#' @export
pairwiseposet<-function(data, formula, verbosity=0)
{
	#First, extract the single variable from formula!
	rhs<-as.character(formula)[3]
	foundVars<-.find.variables(rhs, colnames(data), verbosity=verbosity-1)$varsfound
	actualVars<-colnames(data)[foundVars["any",]]
	
	numvars<-sapply(data[,actualVars], is.numeric)
	ordvars<-sapply(data[,actualVars], is.ordered)
	logvars<-sapply(data[,actualVars], is.logical)
	
	if(any(!(numvars | ordvars | logvars)))
	{
		stop("Variables in right hand side of formula should be a orderable for pairwise poset.")
	}
	
	data <- data[do.call(order, data[,actualVars, drop=FALSE]), ]
	return(onewayposet(data = data, formula=formula, verbosity=verbosity))
}

#' @rdname fullposet
#' 
#' @export
onewayposet<-function(data, formula, verbosity=0)
{
	list(data=data, poset=t(combn(nrow(data),2)))
}

#' @rdname fullposet
#' 
#' @param columnnames column names that will be used to imply order.
#' @export
forcedcolorderonewayposet<-function(columnnames=NULL)
{
	force(columnnames)
	fn<-function(data, formula, verbosity=0)
	{
		if(is.null(columnnames)) columnnames<-colnames(data)
		data <- data[do.call(order, data[,columnnames, drop=FALSE]), ]
		return(onewayposet(data = data, formula=formula, verbosity=verbosity))
	}
	return(fn)
}

#' @rdname fullposet
#' 
#' @export
oldpimposet<-function(data, formula, verbosity=0)
{
	warning("oldpimposet is only provided for compatibility.")
	formula<-stats::formula(formula)
	formula<-as.character(formula)
	resp<-formula[2]
	rhs<-formula[3]
	rhs<-.replace.simple.functiontext(rhs, "O")
	rhs<-.replace.simple.functiontext(rhs, "F")
	rhs<-.replace.simple.functiontext(rhs, "L")
	rhs<-.replace.simple.functiontext(rhs, "R")
	
	maybevars<-.safereordercolnames(colnames(data))
	anyvars<-maybevars[sapply(maybevars, grepl, rhs, fixed=TRUE)]
	
	formula<-stats::formula(paste(resp, rhs, sep="~"))
	trms<-terms(formula)
	vars<-setdiff(names(attr(trms, "variables")), resp)
	trmnames<-attr(trms, "term.labels")
	
	finalformulatxt<-paste(resp, "~", paste(c(vars, anyvars, trmnames), collapse="+"), sep="")
	if(verbosity>0) cat("Formula used to find poset:\"", finalformulatxt, "\"\n")
	formula<-stats::formula(finalformulatxt)
	
	n <- nrow(data)
	mf.tmp <- model.frame(formula = formula, data = data)
	mm.tmp <- model.matrix(object = formula, data = data)
	itcind<-match("(Intercept)", colnames(mm.tmp))
	if(is.na(itcind))
	{
		data.tmp1 <- data.frame(mf.tmp[, 1], mm.tmp)
		names(data.tmp1) <- c(names(mf.tmp)[1], colnames(mm.tmp))
	}
	else
	{
		data.tmp1 <- data.frame(mf.tmp[, 1], mm.tmp[, -itcind])
		names(data.tmp1) <- c(names(mf.tmp)[1], colnames(mm.tmp)[-itcind])
	}
	
	d<-ncol(data.tmp1)
	
	if(verbosity > 0)
	{
		cat("oldpimposet will reorder the data based on these variables:", colnames(data.tmp1)[2:d], "\n")
	}
	data<-data[do.call(order, as.data.frame(data.tmp1[, 2:d])),]
	
	left.tmp <- unlist(mapply(seq, rep(1, (n - 1)), 1:(n - 1)))
	right.tmp <- unlist(mapply(rep, 2:n, 1:(n - 1)))
	
	poset <- cbind(left.tmp, right.tmp)
	return(list(data = data, poset = poset))
}

#' @rdname fullposet
#' 
#' @export
oldpimposetbft<-function(data, formula, verbosity=0)
{
	warning("oldpimposetbft is only provided for compatibility.")
	data <- data[, c(grep(formula[[2]], names(data)), grep(formula[[2]], names(data), invert = TRUE))]
	n <- nrow(data)
	d <- ncol(data) - 1
	
	if(verbosity > 0)
	{
		cat("oldpimposetbft will reorder the data based on these variables:", colnames(data)[-1], "\n")
	}
	data <- data[do.call(order, as.data.frame(data[,-1])),]
	
	left.tmp <- unlist(mapply(seq, rep(1, (n - 1)), 1:(n - 1)))
	right.tmp <- unlist(mapply(rep, 2:n, 1:(n - 1)))
	
	poset <- cbind(left.tmp, right.tmp)
	return(list(data = data, poset = poset))
}