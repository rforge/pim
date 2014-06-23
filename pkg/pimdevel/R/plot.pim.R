#' Plot very basic G.O.F. for a pim
#' 
#' Plot very basic Goodness Of Fit for a pim
#' 
#' @aliases plot.pim
#' 
#' @method plot pim
#' @usage \method{plot}{pim}(x, y, G = 10,...)
#' @param x \code{\link{pim}} object.
#' @param y Ignored currently.
#' @param G Number of groups.
#' @param \dots Ignored currently.
#' @return Nothing.
#' @export
plot.pim<-function (x, y, G = 10, ...) 
{
	if(! exists("Y", x$pfd))
	{
		stop("Cannot plot g.o.f. when the pseudo-observations are no longer present.")
	}
	fv.tmp <- fitted.values(x)
	groups.tmp <- cut(fv.tmp, quantile(fv.tmp, probs = seq(0, 1, 1/G)), include.lowest = T)
	emp.prob <- tapply(x$pfd$Y, groups.tmp, mean)
	model.prob <- tapply(fv.tmp, groups.tmp, mean)
	plot(emp.prob, model.prob, xlim = c(min(emp.prob), max(emp.prob)), 
			 ylim = c(min(model.prob), max(model.prob)), main = "", 
			 lwd = 2, xlab = "", ylab = "")
	title(xlab = "Empirical PI", ylab = "Model PI", font.lab = 1, 
				cex.lab = 1, main = "Goodness-of-fit")
	abline(0, 1, col = 2)
	invisible()
}