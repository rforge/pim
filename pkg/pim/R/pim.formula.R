pim.formula <-
function(formula, data = list(), linkfunction = "logit", boolean.formula = FALSE,  ... )
{
	if(!boolean.formula) 
	{
		tr.tmp <- convert.formula(formula, data)
		formula <- formula(tr.tmp$new.formula)
		data <- tr.tmp$reduced.data
	}else
	{
		data <- data[, c(grep(formula[[2]], names(data)), grep(formula[[2]], names(data), invert = TRUE))]
	}	
	PIM.tmp <- PIM.model.matrix(formula, data) 
	data.tmp <- PIM.tmp$data.tmp

	est <- pim.default(Z = PIM.tmp$Z, PO = data.tmp$PO, gL = data.tmp$groupL, gR = data.tmp$groupR, linkfunction = linkfunction,... )
	est$call <- match.call()
	est$formula <- formula
        est$linkfunction <- linkfunction
	est$PIMdata <- data.tmp
	est$data <- data
        est	
}

