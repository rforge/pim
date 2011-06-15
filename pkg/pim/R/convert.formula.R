convert.formula <-
function(formula, data)
{
	mf.tmp <- model.frame(formula = formula, data = data)	
	mf1 <- model.matrix(formula, data = data)
        mf <- as.matrix(mf1[,-1])	
        colnames(mf) <- colnames(mf1)[-1]
        mf.new <- as.data.frame(model.matrix(formula(paste(names(mf.tmp)[1],"~", paste( names(mf.tmp)[-1],collapse="+"))),mf.tmp))
	names.tmp <- names(mf.new)[-1]
	names.L <- paste(names.tmp,"L",sep="")
	names.R <- paste(names.tmp,"R",sep="")
	termsL <- termsR <- gsub(":", "*", colnames(mf))
	for(i in 1:length(names.tmp))
	{
		termsL <- sub(names.tmp[i], names.L[i],termsL)
		termsR <- sub(names.tmp[i], names.R[i],termsR)
	}
	new.formRHS <- paste(paste(paste("I(", termsR,"-" ,termsL,")",sep=""), collapse = "+"), "-1",sep="")
	new.formLHS <- paste("P(",names(mf.tmp)[1],"L", " < ", names(mf.tmp)[1],"R)",sep="")
	new.form <- paste(new.formLHS, new.formRHS, sep="~")
	data.tmp <- data.frame(mf.tmp[,1],mf.new)[,-2]
        names(data.tmp)[1] <- names(mf.tmp)[1]
	return(list(new.formula = new.form, reduced.data =data.tmp))
}

