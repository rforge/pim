pim.formula <-
function(formula, data = list(), linkfunction = "logit", boolean.formula = FALSE,  ... )
{
	if(!boolean.formula) 
	{
		mf.tmp <- model.frame(formula = formula, data = data)
		mm.tmp <- model.matrix(obj = formula, data = data)
		data.tmp1 <- data.frame(mf.tmp[,1], mm.tmp[,-1]) 
		names(data.tmp1) <- c(names(mf.tmp)[1], colnames(mm.tmp)[-1])
		data.tmp <- PIM.Data(data.tmp1)			
		nc.tmp <- ncol(data.tmp)
		ind1.tmp <- 2:((nc.tmp-3)/2 + 1)
		ind2.tmp <- ((nc.tmp-3)/2 + 2): ((nc.tmp-3) + 1)
		Z <- data.frame(data.tmp[,ind2.tmp] - data.tmp[,ind1.tmp])	
		names(Z) <- names(data.tmp1)[-1]
	}else
	{
		data <- data[, c(grep(formula[[2]], names(data)), grep(formula[[2]], names(data), invert = TRUE))]
		PIM.tmp <- PIM.model.matrix(formula, data) 
		data.tmp <- PIM.tmp$data.tmp
		Z <- PIM.tmp$Z
	}	

	est <- pim.default(Z = Z, PO = data.tmp$PO, gL = data.tmp$group.L, gR = data.tmp$group.R, linkfunction = linkfunction,... )
	est$call <- match.call()
	est$formula <- formula
        est$linkfunction <- linkfunction
	est$PIMdata <- data.tmp
	est$data <- data
	est$Z <- Z
        est	
}

