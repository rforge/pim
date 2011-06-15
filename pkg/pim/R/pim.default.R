pim.default <-
function(Z , PO, gL, gR, linkfunction, logical.formula, ...)
{ 
	Z <- as.matrix(Z)
	PO <- as.numeric(PO)

	est <- pimEst(Z, PO, gL, gR, linkfunction)
	est$call <- match.call()
	class(est) <- "pim"
	est
}

