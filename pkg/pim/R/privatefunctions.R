.find.variables<-function(formulaaschar, maybevars, verbosity=0)
{
	if(verbosity>0) cat("Trying to find any of the variable names", maybevars, "in", formulaaschar)
	regexvars<-maybevars
	regexspecials<-c("\\", ".", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?") #from ?regex
	for(res in regexspecials)
	{
		regexvars<-gsub(res, paste("\\", res, sep=""), regexvars, fixed=TRUE)
	}
	if(verbosity > 0) cat("Regexvars:", regexvars, "\n")
	#For now: assume none of the columnnames are subnames of each other!!
	varsfound<-vapply(regexvars, function(curvar)
	{
		unleftptrn<-paste("([^\\(]|[^L]\\(|^)", curvar, sep="")
		unrightptrn<-paste("([^\\(]|[^R]\\(|^)", curvar, sep="")
		anyptrn<-curvar
		unleftfound<-grepl(unleftptrn, formulaaschar)
		unrightfound<-grepl(unrightptrn, formulaaschar)
		anyfound<-grepl(anyptrn, formulaaschar)
		c(unleft=unleftfound, unright=unrightfound, any=anyfound, includeL=anyfound&unrightfound, includeR=anyfound&unleftfound)
	}, rep(TRUE, 5))
	
	list(varsfound=varsfound, regexvars=regexvars)
}

.filter.poset.blockingvariables<-function(data, poset, blocking.variables)
{
	if(length(blocking.variables)>0)
	{
		#create unique identifiers for the blocks to easily compare them
		#We don't expect more than 5 blocking variables or so, so this should be ok
		if(length(blocking.variables)>1)
		{
			idcols<-lapply(blocking.variables, function(curvar){as.integer(data[,curvar])})
			idcols$sep="_"
			bid<-do.call(paste, idcols)
		}
		else
		{
			bid<-as.integer(data[,blocking.variables])
		}
		withinBlock<-bid[poset[,1]]==bid[poset[,2]]
		poset<-poset[withinBlock,,drop=FALSE]
	}
	return(poset)
}

.LRData<-function(data, poset, left.variables, right.variables)
{
	rn<-rownames(data)
	
	lpart<-data[poset[,1], left.variables$org, drop=FALSE]
	colnames(lpart)<-left.variables$fixed
	rpart<-data[poset[,2], right.variables$org, drop=FALSE]
	colnames(rpart)<-right.variables$fixed
	
	mainPart<-(cbind(lpart, rpart))
	rownames(mainPart)<-paste(rn[poset[,1]], rn[poset[,2]], sep="_")
	return(mainPart)
}

.LRDiffData<-function(data, poset, resp, formula, suffixes)
{
	rn<-rownames(data)
	lhsformula<-stats::formula(paste(resp, "~.", sep=""))
	rhsformula<-update(formula, lhsformula)
	fullmm<-model.matrix(rhsformula, data)
	itccol<-attr(fullmm, "assign")==0
	if(any(itccol))
	{
		fullmm<-fullmm[,!itccol,drop=FALSE]
	}
	outcolnr<-match(resp, colnames(data))
	outcol<-data[,outcolnr]
	newstartleft<-fullmm[poset[,1],,drop=FALSE]
	newstartright<-fullmm[poset[,2],,drop=FALSE]
	outstart<-data.frame(outcol[poset[,1]], outcol[poset[,2]])
	colnames(outstart)<-paste(resp, suffixes, sep="")
	lhsformula<-stats::formula(paste("~", as.character(formula)[2], "-1", sep="" ))
	outmm<-model.matrix(lhsformula, data=outstart)
	Y<-drop(outmm)
	X<-newstartright-newstartleft
	colnames(X)<-colnames(newstartright)
	
	rownames(X)<-paste(rn[poset[,1]], rn[poset[,2]], sep="_")
	return(list(X=X, Y=Y))
}

.basicbootstrap<-function(pfd, verbosity=0)
{
	if(verbosity > 10) cat("original poset:\n")
	if(verbosity > 10)print(pfd$poset)
	n<-max(pfd$poset) #highest row index in original data
	selecteditems<-sample(n, n, replace=TRUE)
	if(verbosity > 10)cat("selecteditems:", selecteditems, "\n")
	
	itemcnts<-tabulate(selecteditems, nbins=n)
	if(verbosity > 10)cat("itemcnts:\n")
	if(verbosity > 10)print(itemcnts)
	
	selectedpo<-itemcnts[pfd$poset[,1]] * itemcnts[pfd$poset[,2]]
	selectedpo<-rep(seq_along(selectedpo), selectedpo)
	if(verbosity > 10)cat("selectedpo:", selectedpo, "\n")
	if(verbosity > 10)cat("total:", sum(selectedpo), "\n")
	
	pfd$Y<-pfd$Y[selectedpo]
	pfd$X<-pfd$X[selectedpo,,drop=FALSE]
	pfd$poset<-pfd$poset[selectedpo,,drop=FALSE]
	pfd$selectedpo<-selectedpo
	return(pfd)
}

.cvpo.glmnet<-function (x, y, poset, weights, offset = NULL, lambda = NULL, type.measure = c("mse", "deviance", "class", "auc", "mae"), 
												..., nfolds = 10, grouped = TRUE, include.extrainfo=FALSE, fullsplit=TRUE, verbosity=0) 
{
	if (missing(type.measure)) 
		type.measure = "default"
	else type.measure = match.arg(type.measure)
	if (!is.null(lambda) && length(lambda) < 2) 
		stop("Need more than one value of lambda for cv.glmnet")
	N = nrow(x)
	if (missing(weights)) 
		weights = rep(1, N)
	else weights = as.double(weights)
	y = drop(y)
	
	tmpx<-x
	tmpy<-y
	tmpweights<-weights
	tmpoffset<-offset
	
	tmpistie<-tmpy==0.5
	if(any(tmpistie))
	{
		warning("Ties found in crossvalidation. Applying weighted design matrix reconstruction.")
		tmpties<-1+tmpistie
		tmptiereps<-rep(seq_along(tmpties), tmpties)
		tmpx<-tmpx[tmptiereps,,drop=FALSE] #repeat the rows with ties twice
		tmpweights<-tmpweights[tmptiereps]/tmpties[tmptiereps] #weight those doubled observations by a half
		tmpmultiplyby<-do.call(c,lapply(tmpistie, function(curtie){if(!curtie) 1 else c(0,2)}))
		tmpy<-tmpy[tmptiereps] * tmpmultiplyby
		
		if(! is.null(tmpoffset)) tmpoffset<-tmpoffset[tmptiereps]
	}
	if(verbosity > 0) cat("Initial glmnet fit\n")
	glmnet.object = glmnet(tmpx, tmpy, weights = tmpweights, offset = tmpoffset, 
												 lambda = lambda, ...)
	is.offset = glmnet.object$offset
	lambda = glmnet.object$lambda
	if (inherits(glmnet.object, "multnet")) {
		nz = predict(glmnet.object, type = "nonzero")
		nz = sapply(nz, function(x) sapply(x, length))
		nz = ceiling(apply(nz, 1, median))
	}
	else nz = sapply(predict(glmnet.object, type = "nonzero"), 
									 length)
	if (nfolds < 3) 
		stop("nfolds must be bigger than 3; nfolds=10 recommended")

	if(verbosity > 0) cat("Fold creation\n")
	uniqueorgindices<-unique(as.vector(poset))
	foldid<-sample(rep(seq(nfolds), length = length(uniqueorgindices)))
	indinfoldidposet<-match(poset, uniqueorgindices)
	foldid<-matrix(foldid[indinfoldidposet], ncol=2)
	orgfoldid<-ifelse(foldid[,1]==foldid[,2], foldid[,1], 0)
	ignoredpo<-orgfoldid==0
	valx<-x[!ignoredpo,,drop=FALSE]
	valy<-y[!ignoredpo]
	valfoldid<-orgfoldid[!ignoredpo]
	valweights<-weights[!ignoredpo]
	valoffset<-offset
	if(! is.null(valoffset)) valoffset<-valoffset[!ignoredpo]
	if(verbosity > 0) cat("Tie correction in folds.\n")
	valistie<-valy==0.5
	if(any(valistie))
	{
		warning("Ties found in crossvalidation. Applying weighted design matrix reconstruction.")
		valties<-1+valistie
		valtiereps<-rep(seq_along(valties), valties)
		valx<-valx[valtiereps,,drop=FALSE] #repeat the rows with ties twice
		valweights<-valweights[valtiereps]/valties[valtiereps] #weight those doubled observations by a half
		valmultiplyby<-do.call(c,lapply(valistie, function(curtie){if(!curtie) 1 else c(0,2)}))
		valy<-valy[valtiereps] * valmultiplyby
		
		valfoldid<-valfoldid[valtiereps]
		if(! is.null(valoffset)) valoffset<-valoffset[valtiereps]
	}
	if(fullsplit)	
	{
		x<-valx
		y<-valy
		foldid<-valfoldid
		weights<-valweights
		offset<-valoffset
	}
	else
	{
		istie<-y==0.5
		if(any(istie))
		{
			warning("Ties found in crossvalidation. Applying weighted design matrix reconstruction.")
			ties<-1+istie
			tiereps<-rep(seq_along(ties), ties)
			x<-x[tiereps,,drop=FALSE] #repeat the rows with ties twice
			weights<-weights[tiereps]/ties[tiereps] #weight those doubled observations by a half
			multiplyby<-do.call(c,lapply(istie, function(curtie){if(!curtie) 1 else c(0,2)}))
			y<-y[tiereps] * multiplyby
			
			if(is.null(dim(foldid)))
			{
				foldid<-foldid[tiereps]
			}
			else
			{
				foldid<-foldid[tiereps,,drop=FALSE]
			}
			if(! is.null(offset)) offset<-offset[tiereps]
		}
	}
	
	outlist = as.list(seq(nfolds))
	for (i in seq(nfolds)) {
		if(verbosity > 1) cat("Fitting glmnet for fold", i, "/", nfolds, "\n")
		if(is.null(dim(foldid)))
		{
			which = foldid == i
		}
		else
		{
			which = (foldid[,1]==i | foldid[,2]==i)
		}
		if (is.matrix(y)) 
			y_sub = y[!which, ]
		else y_sub = y[!which]
		if (is.offset) 
			offset_sub = as.matrix(offset)[!which, ]
		else offset_sub = NULL
		outlist[[i]] = glmnet(x[!which, , drop = FALSE], y_sub, 
													lambda = lambda, offset = offset_sub, weights = weights[!which], 
													...)
	}
	fun = paste("cv", class(glmnet.object)[[1]], sep = ".")
	if(verbosity > 0) cat("Validation\n")
	cvstuff = do.call(fun, list(outlist, lambda, valx, valy, valweights, 
															valoffset, valfoldid, type.measure, grouped))
	if(verbosity > 0) cat("Structuring results\n")
	cvm = cvstuff$cvm
	cvsd = cvstuff$cvsd
	cvname = cvstuff$name
	out = list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + 
		cvsd, cvlo = cvm - cvsd, nzero = nz, name = cvname, glmnet.fit = glmnet.object)
	lamin = if (type.measure == "auc") 
		getmin(lambda, -cvm, cvsd)
	else getmin(lambda, cvm, cvsd)
	obj = c(out, as.list(lamin))
	if(include.extrainfo)
	{
		obj$foldid<-orgfoldid
		obj$foldFits<-outlist
	}
	class(obj) = "cv.glmnet"
	obj
}

.safereordercolnames<-function(nms)
{
	foundcounts<-colSums(sapply(nms, grepl, nms, fixed=TRUE))
	if(sum(foundcounts)==0) return(foundcounts)
	return(nms[order(foundcounts)])
}

.linkfunction<-function(link)
{
	if(link=="identity")
	{
		return(function(x){x})
	}
	else if(link=="logit")
	{
		return(function (x){
			if (sum((x == 0) | (x == 1)) > 0) {
				warning("logit adjustment for numerical instability")
				adjust <- 0.025
				adjust <- abs(adjust)
				x[x == 0] <- adjust
				x[x == 1] <- 1 - adjust
			}
			return(log(x/(1 - x)))
		})
	}
	else if(link=="probit")
	{
		return(qnorm)
	}
	else if(link=="inverse")
	{
		return(function(x){1/x})
	}
	else if(link=="1/mu^2")
	{
		return(function(x){1/x^2})
	}
	else if(link=="log")
	{
		return(log)
	}
	else
	{
		stop("Unsupported link function")
	}
}

.invlinkfunction<-function(link)
{
	if(link=="identity")
	{
		return(function(x){x})
	}
	else if(link=="logit")
	{
		return(function (x){
			underlim <- -700
			upperlim <- 16.81
			if (any((x > upperlim) | (x < underlim))) {
				tmpres <- exp(c(underlim, upperlim))
				tmpres <- tmpres/(1 + tmpres)
				tmpres[2] <- 1 - tmpres[2]
				if (tmpres[1] < tmpres[2]) {
					correctNeeded <- x > 0
					x <- -abs(x)
					if (any(x < underlim)) {
						warning("expit correction for numerical instability")
						x[x < underlim] <- underlim
					}
				}
				else {
					correctNeeded <- x < 0
					x <- abs(x)
					if (any(x > upperlim)) {
						warning("expit correction for numerical instability")
						x[x > upperlim] <- upperlim
					}
				}
			}
			else {
				correctNeeded <- rep(FALSE, length(x))
			}
			tmp <- exp(x)
			tmp <- tmp/(1 + tmp)
			tmp[correctNeeded] <- 1 - tmp[correctNeeded]
			return(tmp)
		})
	}
	else if(link=="probit")
	{
		return(pnorm)
	}
	else if(link=="inverse")
	{
		return(function(x){1/x})
	}
	else if(link=="1/mu^2")
	{
		return(function(x){1/sqrt(x)})
	}
	else if(link=="log")
	{
		return(exp)
	}
	else
	{
		stop("Unsupported link function")
	}
}

.predict<-function(beta, Z, link)
{
	.invlinkfunction(link)(Z %*% beta)
}

.diff.interactions<-function(formula, data, leftsuffix="_L", rightsuffix="_R", nondifferablefunctions=list("F", "O", "L", "R"), verbosity=0)
{
	#note: not taken into account: if an interaction also occurs as a higher order interaction
	#which cannot be differenced, it should not be differenced either! This could then again
	#cause other higher order terms to be non-differable etc...
	#We may provide this later on, but it will required extensive logic
	cf<-as.character(formula)
	
	response<-cf[2]
	response<-gsub("[[:space:]]", "",response) #remove all whitespace
	rhs<-gsub("[[:space:]]", "",cf[3]) #remove all whitespace
	
	trms<-terms(formula, specials=nondifferablefunctions)

	intorder<-attr(trms, "order")
	if(sum(intorder>1) == 0)
	{
		#no interactions: just return the original formula then.
		return(list(formula=as.formula(formula), orglabels=character(), nicelabels=character()))
	}
	inttrms<-which(intorder>1)

	spcls<-attr(trms, "specials")
#	str(spcls)
	skiptrms<-do.call(c, as.list(spcls))
	lit<-length(inttrms)
	inttrms<-setdiff(inttrms, skiptrms)
	if(lit > length(inttrms)) warning("There were interaction terms that cannot be interpreted as a difference.")
	
	if(length(inttrms) == 0)
	{
		#all interactions contain non-differable functions
		return(list(formula=formula, orglabels=character(), nicelabels=character()))
	}
	
	fctrs<-attr(trms, "factors")
	trmlabels<-gsub("[[:space:]]", "", attr(trms, "term.labels"))
	
	varnms<-gsub("[[:space:]]", "", rownames(fctrs))
	fcols<-colnames(data)[apply(data, 2, is.factor)]
	
	maybevars<-colnames(data)
	#If some column names are part of others, we handle the 'bigger' ones first
	maybevars<-.safereordercolnames(maybevars)
	
	newtrms<-lapply(inttrms, function(i){
		lbl<-trmlabels[i]
		partfacts<-fctrs[,i]
		partnames<-varnms[partfacts!=0]
		partfacts<-partfacts[partfacts!=0]
		ord<-intorder[i]
		if(verbosity > 1) cat("Working on (original) interaction term", lbl, "\n")
		#see ?terms: 1 means code by contrasts, 2 means code by _all_ levels
		#Three scenarios are possible:
		#A part is an existing noncategorical column -> just subtract
		#A part is an existing categorical column -> get all combinations
		#A part is a calculated column -> treat the result as a continuous column
		#Note also that the order of the terms does not matter!!
		curtrmrepparts<-lapply(seq_along(partnames), function(pni){
			pn<-partnames[pni]
			pf<-partfacts[pni]
			if(verbosity > 2) cat("\tWorking on part ", pn, "\n")
			if(pn %in% fcols)
			{
				#categorical column
				lvls<-levels(data[,pn])
				if(pf == 1) lvls<-lvls[-1]
				lp<-paste("(as.numeric(L(", pn, ")==\"", lvls, "\"))", sep="")
				rp<-paste("(as.numeric(R(", pn, ")==\"", lvls, "\"))", sep="")
				nn<-paste(pn, lvls, sep="")
			}
			else if(pn %in% maybevars)
			{
				#continuous column
				lp<-paste("L(", pn, ")", sep="")
				rp<-paste("R(",pn, ")", sep="")
				nn<-pn
			}
			else
			{
				#calculated column
				#Nested "I"s may give problems, so try to strip it here
				#We only support the case of "I"s on the outside
				wheresI<-gregexpr("I(", pn, fixed=TRUE)[[1]]
				if(length(wheresI) > 1) stop(paste("Calculated diffed column", pn, "should not contain internal I."))
				if(length(wheresI) == 1)
				{
					if(wheresI > 1) stop(paste("Calculated diffed column", pn, "should not contain internal I."))
					if(substring(pn, nchar(pn))!=")") stop(paste("Calculated diffed column", pn, "should not contain internal I."))
					pn<-substring(pn, 3, nchar(pn)-1) #strip "I(" and ")"
				}
				lp<-rp<-nn<-pn
				for(cn in maybevars)
				{
					lp<-gsub(cn, paste("L(",cn, ")", sep=""), lp)
					rp<-gsub(cn, paste("R(",cn, ")", sep=""), rp)
				}
			}
			return(list(lp=lp, rp=rp, nn=nn))
		})
		leftparts<-do.call(paste,c(do.call(expand.grid, lapply(curtrmrepparts, "[[", "lp")), sep="*"))
		rightparts<-do.call(paste,c(do.call(expand.grid, lapply(curtrmrepparts, "[[", "rp")), sep="*"))
		niceparts<-paste(do.call(paste,c(do.call(expand.grid, lapply(curtrmrepparts, "[[", "nn")), sep=":")), leftsuffix, "-", rightsuffix, sep="")
		
		newtrmparts<-paste("I(", rightparts, "-", leftparts, ")", sep="")
		newtrm<-paste(newtrmparts, collapse="+")
		return(c(newtrm, newtrmparts, niceparts))
	})
	
	trmlabels[inttrms]<-sapply(newtrms, "[", 1)
	orglabels<-do.call(c, lapply(newtrms, function(crtrms){crtrms[1+seq((length(crtrms)-1)/2 )]}))
	nicelabels<-do.call(c, lapply(newtrms, function(crtrms){crtrms[-seq((length(crtrms)+1)/2 )]}))
										 									 
	if(attr(trms, "intercept")==0) trmlabels<-c("-1", trmlabels)
	rhs<-paste(trmlabels, collapse="+")
	if(verbosity>0)
	{
		cat("New right hand side:\n")
		cat(rhs, "\n\n")
		cat("Original labels to be replaced:\n")
		print(data.frame(orglabels=orglabels, nicelabels=nicelabels))
	}
	#return(list(formula=c("~", response, paste(trmlabels, collapse="+")), orglabels=orglabels, nicelabels=nicelabels))
	return(list(formula=stats::formula(paste(response,"~", rhs,sep="")), orglabels=orglabels, nicelabels=nicelabels))
}

.replace.simple.functiontext<-function(txt, fn)
{
	pattern<-paste(fn, "\\(([^\\)]*)\\)", sep="")
	gsub(pattern, "\\1", txt)
}

.pimformula.difference<-function(formula, data, verbosity, leftsuffix, rightsuffix, extra.variables, lhs,lhsreplacer)
{
	maybevars<-colnames(data)
	if(verbosity > 0) cat("Colnames:", maybevars, "\n")
	
	#First, do a sanity check on the columns:
	lvars<-paste(maybevars, leftsuffix, sep="")
	if(any(lvars %in% maybevars)) stop("The left suffix cause contradicting column names. This is not supported.")
	rvars<-paste(maybevars, rightsuffix, sep="")
	if(any(rvars %in% maybevars)) stop("The right suffix cause contradicting column names. This is not supported.")
	
	#If some column names are part of others, we handle the 'bigger' ones first
	maybevars<-.safereordercolnames(maybevars)

	tmpdata<-data[1,,drop=FALSE]
	mm<-model.matrix(formula, data=tmpdata)
	nicenames<-colnames(mm)
	itccol<-attr(mm, "assign")==0
	if(any(itccol)) nicenames<-nicenames[!itccol]
	
	response<-gsub("[[:space:]]", "",(as.character(formula))[2]) #remove all whitespace
	rhs<-gsub("[[:space:]]", "",(as.character(formula))[3])
	
	tmp<-.find.variables(rhs, maybevars, verbosity=verbosity)
	regexvars<-tmp$regexvars
	varsfound<-tmp$varsfound
	actualvars<-maybevars[varsfound["any",]]
	
	left.variables<-unique(c(actualvars, response, extra.variables))
	left.variables<-data.frame(org=left.variables, fixed=left.variables, stringsAsFactors=FALSE)
	right.variables<-left.variables
	if(nrow(left.variables) > 0)
	{
		left.variables$fixed<-paste(left.variables$fixed, leftsuffix, sep="")
		right.variables$fixed<-paste(right.variables$fixed, rightsuffix, sep="")
	}

	ort<-lhsreplacer(response, data=data, verbosity=0, leftsuffix=leftsuffix, rightsuffix=rightsuffix, lhs=lhs)
	newresponse<-gsub(ort[1], ort[2], response, fixed=TRUE)
	newformula<-update(formula, stats::formula(paste(newresponse, "~.", sep="")))
	#nms<-attr(terms(newformula), "term.labels")
	
	retval<-list(newformula=newformula, left.variables=left.variables, right.variables=right.variables, 
							 names=nicenames, full.colnames=nicenames, nice.colnames=nicenames, orgresp=response) 
	
	class(retval)<-"pimformula"
	return(retval)
}

.toIntCol<-function(col, data)
{
	as.integer(as.factor(data[[col]]))
}

.handleError<-function(errTxt, treat.error=c("warn", "error", "log", "ignore"))
{
	treat.error<-match.arg(treat.error)
	if(treat.error == "log")
	{
		cat(errTxt, "\n")
	}
	else if(treat.error == "ignore")
	{
		#just do nothing
	}
	else if(treat.error == "error")
	{
		stop(errTxt)
	}
	else
	{
		warning(errTxt)
	}
	invisible()
}