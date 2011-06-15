plot.pim <-
function(object, G=10, ...)
{
	fv.tmp <- fitted.values(object)
	groups.tmp <- cut(fv.tmp, quantile(fv.tmp, probs =   seq(0,1, 1/G)), include.lowest = T) 
	emp.prob <- tapply(object$PIMdata$PO, groups.tmp, mean)
	model.prob <- tapply(fv.tmp, groups.tmp, mean)
	plot(emp.prob, model.prob, xlim=c(min(emp.prob),max(emp.prob)) ,ylim=c(min(model.prob),max(model.prob)),main="",lwd=2,xlab="",ylab="")
	title(xlab="Empirical PI",ylab="Model PI",font.lab=1,cex.lab=1,main="Goodness-of-fit")
	abline(0,1,col=2)
}

