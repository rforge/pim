print.summary.pim <-
function(x,...)
{
	cat("\n")
	cat("Call:\n")
	print(x$call)
	cat("\n")
	printCoefmat(x$coefficients, P.value = TRUE, has.Pvalue = TRUE)
}

