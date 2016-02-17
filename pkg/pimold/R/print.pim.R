#' Display a PIM
#' 
#' Display a PIM
#' 
#' @aliases print.pim
#' 
#' @method print pim
#' @usage \method{print}{pim}(x,...)
#' @param x \code{\link{pim}} object.
#' @param \dots Ignored currently.
#' @return Nothing.
#' @export
print.pim<-function(x, ...)
{
	cat("\nCall:\n")
	print(x$call)
	cat("\nCoefficients:\n")
	print(x$coefficients)
# 	if(exists("vcov", x))
# 	{
# 		cat("\nCovariance estimates.\n")
# 		print(x$vcov)
# 	}
# 	else
# 	{
# 		cat("\nNo covariance estimates.\n")
# 	}
	invisible()
}
