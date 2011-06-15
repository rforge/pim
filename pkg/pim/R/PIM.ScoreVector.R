PIM.ScoreVector <-
function(Zbeta, Z, PO, linkfunction)
{
	Zbeta <- c(Zbeta)
	PO <- c(PO)
	if(linkfunction == "probit")
	{
		fv <- pnorm(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI)
		m.d <- dnorm(Zbeta)
		m.dd <- -m.d*Zbeta 
		res <- PO-fv
		U <- Z*m.d*res/var.PI
		U.diff <- t(Z)%*%(Z*c((var.PI*(m.dd*res - m.d^2) - res*m.d^2*(1-2*fv))/var.PI^2))
	}else
	{
		fv <- plogis(Zbeta)
		var.PI <- fv*(1-fv)
		var.PI <- ifelse(var.PI==0,0.01,var.PI)
		U <- Z*c(PO-fv)
		U.diff <- -t(Z)%*%(Z*c(var.PI))
	}
	return(list(U, U.diff, fv))
}

