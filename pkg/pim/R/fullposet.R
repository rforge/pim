#' Get a set of pseudo-observation observation indices
#' 
#' Get a set of pseudo-observation observation indices
#' 
#' @aliases poset fullposet noselfposet lexiposet onewayposet forcedcolorderonewayposet
#' 
#' @param data Context where the formula \code{formula} is to be interpreted
#' @param formula Original formula
#' @param weights Vector of weights per row of \code{data} or \code{NULL}.
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @return List with 3 items:
#' \item{data}{Similar to the passed along \code{data}, but may contain fewer rows.} 
#' \item{poset }{Matrix of two columns indicating what the original observation number is
#' 	for the left and right real observation in the pseudo-observation.} 
#' \item{weights }{Weight to be applied to each row of the dataset. Should contain one
#' 		weight per row of data (and match its order) or equal \code{NULL}. Important: if
#' 		order of data is changed/rows are left out: update weights as well!} 
#' @details The provided implementations differ as follows:
#' \enumerate{
#' \item \code{fullposet} Contains all combinations of rowindices.
#' \item \code{noselfposet} The same as fullposet, but excluding the rowcombinations 
#' 	with identical indexes.
#' \item \code{lexiposet} Check that predicting variables can be ordered, and select 
#' 	only combinations where the predictors are bigger on the right side. 
#' \item \code{onewayposet} Similar to \code{lexiposet}, but simply uses row index.
#' \item \code{forcedcolorderonewayposet} First reorders the data based on a set of given 
#' 	column names. Note: the other functions here are to be passed along as 
#' 	\code{poset=fullposet}, this one needs \code{poset=forcedcolorderonewayposet(c("col1", "col2"))}
#' \item \code{forcedposet} Starts from all observations, but excludes the ones where
#' 	\itemize{
#' 		\item the combinations of the columns in \code{diffcols} are the same in left and right observation
#' 		\item the combinations of the columns in \code{LLessRcols} in the left observation 
#' 			are greater than the one in the right observation (equals are dropped too if \code{no.equals=TRUE})
#' 		\item the combinations of the columns in \code{RLessLcols} in the right observation 
#' 			are greater than the one in the left observation (equals are dropped too if \code{no.equals=TRUE})
#' 	}
#' 	As a side effect, the data is also sorted wrt the \code{LLessRcols} (ascending) and then according to
#' 	 the \code{RLessLcols} (descending).
#' \item \code{oldpimposet} and \code{oldpimposetbft} Sorts the data according to all 
#' 	predictors, then does oneway-style. These are mainly provided for comparison to a
#' 	previous implementations of pim.
#' }
#' @seealso \code{\link{pim}}
#' @keywords pim fit poset
#' @examples set.seed(1)
#' 	dta<-data.frame(y=runif(6), x=ordered(sample(3, 6, replace=TRUE)))
#' 	fullposet(dta, y~x)$poset
#' 	lexiposet(dta, y~x)$poset
#' 	onewayposet(dta, y~x)$poset
#' @export
fullposet<-function(data, formula, weights=NULL, verbosity=0)
{
	n<-nrow(data)
	
	org<-seq(n)
	poset<-cbind(rep(org, n), rep(org, each=n))
	return(list(data=data, poset=poset, weights=weights))
}

#' @rdname fullposet
#' 
#' @export
noselfposet<-function(data, formula, weights=NULL, verbosity=0)
{
	n<-nrow(data)
	rv<-fullposet(data, formula, weights=weights, verbosity)
	selfrefs<-(seq(n)*(n+1))-n
	rv$poset<-rv$poset[-selfrefs,]
	return(rv)
}

#' @rdname fullposet
#' 
#' @export
lexiposet<-function(data, formula, weights=NULL, verbosity=0)
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
		stop("Variables in right hand side of formula should be a orderable for lexicographical poset.")
	}
	
	if(verbosity>0)
	{
		cat("lexiposet will reorder the data based on these variables:", actualVars, "\n")
	}
	ordr<-do.call(order, data[,actualVars, drop=FALSE])
	data <- data[ordr, ]
	if(!is.null(weights)) weights<-weights[ordr]
	return(onewayposet(data = data, formula=formula, weights=weights, verbosity=verbosity))
}

#' @rdname fullposet
#' 
#' @export
onewayposet<-function(data, formula, weights=NULL, verbosity=0)
{
	list(data=data, poset=t(combn(nrow(data),2)), weights=weights)
}

#' @rdname fullposet
#' 
#' @param columnnames column names that will be used to imply order.
#' @export
forcedcolorderonewayposet<-function(columnnames=NULL)
{
	force(columnnames)
	fn<-function(data, formula, weights=NULL, verbosity=0)
	{
		if(is.null(columnnames)) columnnames<-colnames(data)
		ordr<-do.call(order, data[,columnnames, drop=FALSE])
		data <- data[ordr, ]
		if(!is.null(weights)) weights<-weights[ordr]
		return(onewayposet(data = data, formula=formula, weights=weights, verbosity=verbosity))
	}
	return(fn)
}

#' @rdname fullposet
#' 
#' @param diffcols column names that will be used to imply order.
#' @param LLessRcols column names where you want the left observation to be smaller.
#' @param RLessLcols column names where you want the right observation to be smaller
#' @param no.equals if \code{TRUE} (the default), rows that have equal values in the
#' 	\code{LLessRcols} or \code{RLessLcols} columns are excluded from being combined.
#' @export
forcedposet<-function(diffcols=NULL, LLessRcols=NULL, RLessLcols=NULL, no.equals=TRUE)
{
	force(diffcols)
	force(LLessRcols)
	force(RLessLcols)
	fn<-function(data, formula, weights=NULL, verbosity=0)
	{
		ps<-fullposet(data, formula, verbosity-1)
		ocols<-list()
		if(!is.null(LLessRcols))
		{
			ocols<-lapply(LLessRcols, .toIntCol, data)
		}
		if(!is.null(RLessLcols))
		{
			ocols<-c(ocols, lapply(lapply(RLessLcols, .toIntCol, data), function(x){-x}))
		}
		l<-nrow(data)
		if(length(ocols)>0)
		{
			#First, we order the data according to the given columns
			ordr<-do.call(order, ocols)
			data <- data[ordr,]
			if(!is.null(weights)) weights<-weights[ordr]
			if(no.equals)
			{
				#Now, we simply have to avoid the rows where both values are equal
				#Because of the order, we only have to check consecutive rows!
				uniqueid<-do.call(paste, c(ocols, sep="_"))
				equalsNext<-uniqueid[-length(uniqueid)]==uniqueid[-1]
				allowed<-lapply(seq_along(equalsNext), function(rownr){
					if(equalsNext[rownr])
					{
						firstNonEqual<-rownr+1+match(FALSE, equalsNext[-seq(1,rownr)])
					}
					else
					{
						firstNonEqual<-rownr+1
					}
					#For each rownr, return the rownrs that have a value higher
					return (if(is.na(firstNonEqual)) integer() else as.integer(seq(firstNonEqual, l)))
				})
			}
			else
			{
				#For each rownr, return all higher rownrs
				allowed<-lapply(seq(2,l), seq, l)
			}
		}
		else
		{
			#For each rownr, return all other rownrs
			sl<-seq(l)
			allowed<-lapply(sl, function(nr){sl[-nr]})
		}
		
		if(!is.null(diffcols))
		{
			uniqueid<-do.call(paste, c(lapply(diffcols, .toIntCol, data), sep="_"))
			allowed<-lapply(seq(length(allowed)), function(rownr){
				if(!is.null(allowed[[rownr]]))
				{
					disallowed<-which(uniqueid==uniqueid[rownr])
					allowed[[rownr]]<-setdiff(allowed[[rownr]], disallowed)
				}
				return(allowed[[rownr]])
			})
		}
		
		lobs<-rep(seq_along(allowed), sapply(allowed, length))
		robs<-do.call(c, allowed)
		
		return(list(data = data, poset = cbind(lobs, robs), weights=weights))
	}
	return(fn)
}
