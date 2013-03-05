#' Pseudo-observation formula conversion helpers
#' 
#' Pseudo-observation formula conversion helpers
#' 
#' @aliases Mainreplacetext LHSreplacetext Freplacetext Oreplacetext Lreplacetext Rreplacetext
#' 
#' @param varn \code{character} holding the variable (column) name that will be considered.
#' @param data Context where the column name can be interpreted
#' @param verbosity The higher this value, the more levels of progress and debug 
#' information is displayed (note: in R for Windows, turn off buffered output)
#' @param leftsuffix,rightsuffix Suffixes that will be added to the 'left' and 'right'
#' 	observation's column name in the pseudo-observation. Note: no checking is done that 
#' 	these suffixes are safe, so the wrong suffixes may lead to unexpected behaviour.
#' @param interpretation If \code{"marginal"} (not the default) parts of the formula are 
#' 	converted to imply marginal pim modeling (see e.g. \code{\link{Mainreplacetext}}). If
#' 	it is \code{"difference"}, then the design matrix of the PIM is the difference of the
#' 	design matrices of each part of the pseudo-observations. The default option is
#' 	\code{"regular"}, which will interpret unaltered columns as differences. A new option
#' 	is \code{"symmetric"}, which works the same as \code{"regular"}, but will enforce
#' 	the symmetry condition by making the sign switch when changing the order (typically,
#' 	this is achieved by subtracting the inverse for each dummy).
#' @param lhs \code{"PO"}, \code{"<"} or \code{"<="}: Unequality used for the lefthandside of 
#' 	the formula. The default (\code{"PO"}) is the normal probabilistic index.
#' @return A vector of at least two \code{character} values. The first one is the "find" text, the 
#' 	second one holds the "replace" text. Depending on the function, this may or may not hold
#' 	regular expressions.
#' 	What comes after these first two items are more character values. the first half of these
#' 	represent the expected names of the resulting columns in the design matrix, the second
#' 	half are (in the same order) somewhat cleaned up names for them.
#' @details Each of these functions (except \code{Mainreplacetext}) takes parts of a formula
#' 	like \code{"L(somevar)"} and turns it into the matching formula text for pseudo-observations.
#' 	
#' 	The replace ment texts currently look like this for each of the functions (assuming the 
#' 	default suffixes, and where relevant \code{X} a three-level (ordered) factor):
#' 	\itemize{
#' 	\item{\code{X}} becomes \code{X_R} if \code{interpretation} is \code{"marginal"}, and \code{I(as.numeric(X_R)-as.numeric(X_L))} otherwise
#' 	\item{\code{F(X)}} becomes \code{(I(as.numeric(X_R=="lvl1"))+I(as.numeric(X_R=="lvl2"))+I(as.numeric(X_R=="lvl3")))}  
#' 		if \code{interpretation} is \code{"marginal"}, and \code{( I(as.numeric(X_L=="lvl1"&X_R=="lvl2"))+I(as.numeric(X_L=="lvl1"&X_R=="lvl3"))+I(as.numeric(X_L=="lvl2"&X_R=="lvl3")))} otherwise
#' 	\item{\code{O(X)}} becomes \code{I(as.numeric(as.numeric(X_L)<as.numeric(X_R)))}
#' 	\item{\code{L(X)}} becomes \code{X_L}
#' 	\item{\code{R(X)}} becomes \code{X_R}
#' 	\item{\code{Y} on the left hand side} becomes \code{I(as.numeric(as.numeric(X_L)<as.numeric(X_R)))}, 
#' 		\code{I(as.numeric(as.numeric(X_L)==as.numeric(X_R)))} or
#' 		\code{I(as.numeric(as.numeric(X_L)<as.numeric(X_R)) + 0.5*as.numeric(as.numeric(X_L)==as.numeric(X_R)))}
#' 		depending on \code{lhs}
#' 	}
#' 	Please note that these replacements will only work for column names present in \code{data}, and not for 
#' 	"calculated" columns. Trying this will likely result in unpredictable behaviour or errors.
#' @seealso \code{\link{pimformula}}
#' @keywords pim formula replace
#' @examples iris$Spord<-as.ordered(iris$Species)
#' Mainreplacetext("Sepal.Length", iris, verbosity=1)
#' LHSreplacetext("Sepal.Length", iris, verbosity=1)
#' Freplacetext("Species", iris, verbosity=1)
#' Oreplacetext("Spord", iris, verbosity=1)
#' Lreplacetext("Sepal.Length", iris, verbosity=1)
#' Rreplacetext("Sepal.Length", iris, verbosity=1)
#' Mainreplacetext("Sepal.Length", iris, verbosity=1, interpretation="marginal")
#' LHSreplacetext("Sepal.Length", iris, verbosity=1, interpretation="marginal")
#' Freplacetext("Species", iris, verbosity=1, interpretation="marginal")
#' Oreplacetext("Spord", iris, verbosity=1, interpretation="marginal")
#' Lreplacetext("Sepal.Length", iris, verbosity=1, interpretation="marginal")
#' Rreplacetext("Sepal.Length", iris, verbosity=1, interpretation="marginal")
#' @export
Mainreplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
													interpretation=c("regular", "difference", "marginal", "symmetric"))
{
	interpretation<-match.arg(interpretation)
	regexvar<-varn
	regexspecials<-c("\\", ".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?") #from ?regex
	for(res in regexspecials)
	{
		regexvar<-gsub(res, paste("\\", res, sep=""), regexvar, fixed=TRUE)
		leftsuffix<-gsub(res, paste("\\", res, sep=""), leftsuffix, fixed=TRUE)
		rightsuffix<-gsub(res, paste("\\", res, sep=""), rightsuffix, fixed=TRUE)
	}
	
	patpart<-paste(regexvar, "(?!", leftsuffix, "|", rightsuffix, ")", sep="")
	
	if(interpretation=="marginal")
	{
		reppart<-paste(varn, rightsuffix, sep="")
		nicename<-reppart
	}
	else
	{
		if(is.numeric(data[,varn]))
		{
			reppart<-paste("I(", varn, rightsuffix, "-", varn, leftsuffix, ")", sep="")
			nicename<-paste(varn, rightsuffix,"-",leftsuffix, sep="")
		}
		else
		{
			lvls<-levels(data[,varn])[-1] #Here, we skip the first level as being the reference #Is this right??
			parts<-paste("I(as.numeric(", varn, rightsuffix, "==\"", as.character(lvls), "\") - as.numeric(", varn, leftsuffix, "==\"", as.character(lvls), "\"))", sep="")
			reppart<-paste("(", paste(parts, collapse="+"), ")", sep="")
			nicename<-paste(varn, rightsuffix, "-", leftsuffix, sep="")
			nicename<-paste(nicename,"_", as.character(lvls), sep="")
			return(c(patpart, reppart, parts, nicename))
		}
	}
	
	return(c(patpart, reppart, reppart, nicename))
}

#' @rdname Mainreplacetext
#' 
#' @export
LHSreplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
												 interpretation=c("regular", "difference", "marginal", "symmetric"), lhs=c("PO", "<", "<="))
{
	interpretation<-match.arg(interpretation)
	lhs<-match.arg(lhs)
	if(lhs=="<")
	{
		reppart<-paste("I(as.numeric(as.numeric(", varn, leftsuffix, ")<as.numeric(", varn, rightsuffix, ")))", sep="")
	}
	else if(lhs=="<")
	{
		reppart<-paste("I(as.numeric(as.numeric(", varn, leftsuffix, ")<=as.numeric(", varn, rightsuffix, ")))", sep="")
	}
	else
	{
		reppart<-paste("I(as.numeric(as.numeric(", varn, leftsuffix, ")<as.numeric(", varn, rightsuffix, "))+0.5*as.numeric(as.numeric(", varn, leftsuffix, ")==as.numeric(", varn, rightsuffix, ")))", sep="")
	}
	nicename<-paste(varn, leftsuffix, " ", lhs, " ", varn, rightsuffix, sep="")
	
	return(c(varn, reppart, reppart, nicename))
}

#' @rdname Mainreplacetext
#' 
#' @export
Freplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
											 interpretation=c("regular", "difference", "marginal", "symmetric"))
{
	interpretation<-match.arg(interpretation)
	lvls<-levels(data[[varn]])
	if(is.null(lvls))
	{
		if(verbosity>0) cat("Should not apply F to nonfactor variable", varn, "\n")
		lvls<-c(0,1) #have to do something in this case
	}
	
	if(interpretation=="marginal")
	{
		#Should we perhaps exclude the first level (reference??) Probably not because we only look at it on one side!.
		parts<-paste("I(as.numeric(", varn, rightsuffix, "==\"", as.character(lvls), "\"))", sep="")
		reppart<-paste("(", paste(parts, collapse="+"), ")", sep="")
		nicename<-paste(varn, rightsuffix, sep="")
		nicename<-paste(nicename,"_", as.character(lvls), sep="")
	}
	else if(interpretation=="symmetric")
	{
		uselvls<-combn(lvls, 2)
		parts<-paste("I((as.numeric(", varn, leftsuffix, "==\"", uselvls[1,], "\"&",varn, rightsuffix, "==\"", uselvls[2,], 
								 "\"))+(as.numeric(", varn, rightsuffix, "==\"", uselvls[1,], "\"&",varn, leftsuffix, "==\"", uselvls[2,], "\")))", sep="")
		reppart<-paste("(", paste(parts, collapse="+"), ")", sep="")
		nicename<-paste(varn,leftsuffix, rightsuffix, sep="")
		nicename<-paste(nicename,"_", uselvls[1,], ",", uselvls[2,], "-", uselvls[2,], ",", uselvls[1,], sep="")
	}
	else
	{
		uselvls<-combn(lvls, 2)
		parts<-paste("I(as.numeric(", varn, leftsuffix, "==\"", uselvls[1,], "\"&",varn, rightsuffix, "==\"", uselvls[2,], "\"))", sep="")
		reppart<-paste("(", paste(parts, collapse="+"), ")", sep="")
		nicename<-paste(varn,leftsuffix, rightsuffix, sep="")
		nicename<-paste(nicename,"_", uselvls[1,], "_", uselvls[2,], sep="")
	}
	
	return(c(paste("F(", varn, ")", sep=""), reppart, parts, nicename))
}

#' @rdname Mainreplacetext
#' 
#' @export
Oreplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
											 interpretation=c("regular", "difference", "marginal", "symmetric"))
{
	interpretation<-match.arg(interpretation)
	if(interpretation=="marginal")
	{
		if(verbosity>0) cat("Should not use ordered version of variable", varn, "in a marginal PIM.\n")
	}
	if(!is.ordered(data[[varn]]))
	{
		if(verbosity>0) cat("Should not apply O to non-ordered variable", varn, "\n")
	}
	
	if(interpretation=="marginal")
	{
		reppart<-paste("I((as.numeric(as.numeric(", varn, leftsuffix, ")<as.numeric(", varn, rightsuffix, 
									 ")))-(as.numeric(as.numeric(", varn, rightsuffix, ")<as.numeric(", varn, leftsuffix, "))))", sep="")
		nicename<-paste(varn, leftsuffix,"<",rightsuffix,"-", rightsuffix,"<",leftsuffix, sep="")
	}
	else
	{
		reppart<-paste("I(as.numeric(as.numeric(", varn, leftsuffix, ")<as.numeric(", varn, rightsuffix, ")))", sep="")
		nicename<-paste(varn, leftsuffix,"<",rightsuffix, sep="")
	}
	
	return(c(paste("O(", varn, ")", sep=""), reppart, reppart, nicename))
}

#' @rdname Mainreplacetext
#' 
#' @export
Lreplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
											 interpretation=c("regular", "difference", "marginal", "symmetric"))
{
	interpretation<-match.arg(interpretation)
	if(interpretation=="marginal")
	{
		if(verbosity>0) cat("Should not use left version of variable", varn, "in a marginal PIM.\n")
	}
	reppart<-paste(varn, leftsuffix, sep="")
	
	repname<-reppart
	if(is.factor(data[[varn]]))
	{
		lvls<-levels(data[[varn]])
		repname<-paste(repname, lvls, sep="")
	}
	
	return(c(paste("L(", varn, ")", sep=""), reppart, repname, repname))
}

#' @rdname Mainreplacetext
#' 
#' @export
Rreplacetext<-function(varn, data, verbosity=0, leftsuffix="_L", rightsuffix="_R", 
											 interpretation=c("regular", "difference", "marginal", "symmetric"))
{
	interpretation<-match.arg(interpretation)
	reppart<-paste(varn, rightsuffix, sep="")
	
	repname<-reppart
	if(is.factor(data[[varn]]))
	{
		lvls<-levels(data[[varn]])
		repname<-paste(repname, lvls, sep="")
	}
	
	
	return(c(paste("R(", varn, ")", sep=""), reppart, repname, repname))
}

