PIM.Solve <-
function(Z, PO, linkfunction)
{
	if (!exists("nleqslv")) require(nleqslv) # JM : when an import is specified, no need to do this
	if(linkfunction == "logit" | linkfunction == "probit") 
		fit.x <- nleqslv(rep(0,ncol(Z)), PIM.ScoreFunction(Z, PO, linkfunction), Z=Z, PO=PO)$x
	else stop("linkfunction must be 'logit' or 'probit'.")
	names(fit.x) <- colnames(Z) 
	return(fit.x)
}

