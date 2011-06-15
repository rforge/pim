summary.pim <-
function(object,...)
{	
	se <- sqrt(diag(object$vcov))
	zval <- coef(object)/se
	TAB <- cbind(Estimate = coef(object), "Std. Error" = se, "Z value" = zval, "Pr(>|z|)" = 2*pnorm(-abs(zval)))
	res <- list(call = object$call, coefficients = TAB) 
	class(res) <- "summary.pim"
	res
}

