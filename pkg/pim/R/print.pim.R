print.pim <-
function(x,...)
{
	cat("\nCall:\n")
	print(x$call)
	cat("\nCoefficients:\n")
	print(x$coefficients)
}

