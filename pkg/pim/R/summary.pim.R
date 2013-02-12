#' Plot very basic G.O.F. for a pim
#' 
#' Plot very basic Goodness Of Fit for a pim
#' 
#' @aliases summary.pim summary.pim-class print.summary.pimo vcov.pim
#' 
#' @method summary pim
#' @usage \method{summary}{pim}(object,...)
#' @param object \code{\link{pim}} object.
#' @param \dots Ignored currently.
#' @return For \code{summary.pim}: an object of class \code{summary.pim}. It holds the original call in an item call, 
#' 	and has an additional item coefficients, that is a matrix holding columns:
#' 	\item{Estimate}{The coefficient estimate.}
#' 	\item{Std. Error}{Their standard error.}
#' 	\item{Z value}{The standardized value (Test statistic for true coefficient zero).}
#' 	\item{Pr(>|z|)}{The p-value.}
#' @examples set.seed(1)
#' 	myiris<-iris
#'	myiris$xord<-ordered(sample(letters[1:3], nrow(myiris), replace=TRUE))
#'	myiris$out<-runif(nrow(myiris))
#'	
#'	irisprt<-myiris[sample(nrow(myiris), 10),] #10 random rows from iris
#'	pim1<-pim(out~Sepal.Length, data=irisprt, link="logit")
#'	summary(pim1)
#'	vcov(pim1)
#'	design.matrix(pim1)
#'	responses(pim1)

#' @export
summary.pim <- function(object,...)
{	
	se <- sqrt(diag(object$vcov))
	zval <- coef(object)/se
	TAB <- cbind(Estimate = coef(object), "Std. Error" = se, "Z value" = zval, "Pr(>|z|)" = 2*pnorm(-abs(zval)))
	res <- list(call = object$call, coefficients = TAB) 
	class(res) <- "summary.pim"
	res
}

#' @rdname summary.pim
#' 
#' @method print summary.pim
#' @usage \method{print}{summary.pim}(x,...)
#' @param x \code{summary.pim} object.
#' @return For \code{print.summary.pim}: invisibly returns \code{x}
#' @export
print.summary.pim <-function(x,...)
{
	cat("\n")
	cat("Call:\n")
	print(x$call)
	cat("\n")
	printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE)
}

#' @rdname summary.pim
#' 
#' @method vcov pim
#' @usage \method{vcov}{pim}(object,...)
#' @return For \code{vcov.pim}: a (co)variance matrix
#' @export
vcov.pim <-function(object,...)
{
	object$vcov
}

#' @rdname summary.pim
#' 
#' @aliases design.matrix
#' @return For \code{design.matrix}: The design matrix (of pseudo-observations) 
#' @export
design.matrix<-function(object,...) UseMethod("design.matrix")

#' @rdname summary.pim
#' 
#' @aliases design.matrix.pim
#' @method design.matrix pim
#' @usage \method{design.matrix}{pim}(object,...)
#' @export
design.matrix.pim<-function(object,...)
{
	if(exists("X", object$pfd))
	{
		return(object$pfd$X)
	}
	warning("design.matrix cannot be obtained if keep.data was FALSE.")
	return(NULL)
}

#' @rdname summary.pim
#' 
#' @aliases design.matrix
#' @return For \code{responses}: The true responses (pseudo-observations) 
#' @export
responses<-function(object,...) UseMethod("responses")

#' @rdname summary.pim
#' 
#' @aliases responses.pim
#' @method responses pim
#' @usage \method{responses}{pim}(object,...)
#' @export
responses.pim<-function(object,...)
{
	if(exists("Y", object$pfd))
	{
		return(object$pfd$Y)
	}
	warning("responses cannot be obtained if keep.data was FALSE.")
	return(NULL)
}

#' @rdname summary.pim
#' 
#' @aliases designcols
#' @return For \code{designcols}: The orginal column names of the design matrix.
#' 	These typically also contain the part of the formula used to build them, so 
#' 	indicate their source.
#' @export
designcols<-function(object,...) UseMethod("designcols")

#' @rdname summary.pim
#' 
#' @aliases designcols.pim
#' @method designcols pim
#' @usage \method{designcols}{pim}(object,...)
#' @export
designcols.pim<-function(object,...)
{
	return(object$pfd$original.colnames)
}

#' @rdname summary.pim
#' 
#' @aliases outcomeformula
#' @return For \code{outcomeformula}: The left hand side of the formula used to.
#' 	create the design matrix (i.e. to generate the pseudo-outcomes).
#' @export
outcomeformula<-function(object,...) UseMethod("outcomeformula")

#' @rdname summary.pim
#' 
#' @aliases outcomeformula.pim
#' @method outcomeformula pim
#' @usage \method{outcomeformula}{pim}(object,...)
#' @export
outcomeformula.pim<-function(object,...)
{
	return(object$pfd$pimformula$newformula[2])
}

