pimEst <-
function(Z, PO, gL, gR, linkfunction)
{
	fit.x <- PIM.Solve(Z, PO, linkfunction)
	SV.tmp <- PIM.ScoreVector(Z%*%fit.x, Z, PO, linkfunction) 
	varcov <-   PIM.SandwichEstimator(SV.tmp[[1]], SV.tmp[[2]], g1 = gL, g2 = gR)
	rownames(varcov) <- colnames(varcov) <- names(fit.x)
	list( coefficients = fit.x,
        vcov = varcov,
        fitted.values = SV.tmp[[3]],
        U = SV.tmp[[1]],
        U.der = SV.tmp[[2]]
 )
}

