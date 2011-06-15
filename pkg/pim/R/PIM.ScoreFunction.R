PIM.ScoreFunction <-
function(Z, PO, linkfunction)
{
	if(linkfunction == "probit"){
		U.func <- function(beta, Z, PO){ 
		Zbeta <- c(Z%*%beta)
		colSums(Z*dnorm(Zbeta)*c(PO - pnorm(Zbeta))/c(pnorm(Zbeta)*(1-pnorm(Zbeta))))}
	}else
	{
		U.func <- function(beta, Z, PO) colSums(Z*c(PO - plogis(Z%*%beta)))
	}
	return(U.func)
}

