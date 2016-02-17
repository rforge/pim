#' The summary function for the pim class
#' 
#' The function \code{\link[base]{summary}} is a generic function. We provide
#' a method for objects of the \code{\link{pim-class}}. 
#' 
#' @param object an object of the class pim.
#' @param ... arguments passed to other methods. Currently ignored.
#' 
#' @examples 
#' data(FEVData)
#' Model <- pim(FEV~ Age + Smoke*Sex , data=FEVData)
#' summary(Model)
#' 
#' @rdname summary.pim
#' @aliases summary
#' @export
setGeneric('summary')

summary.pim <- function(object){
  coefs <- coef(object)
  se <- diag(vcov(object))
  zval <- coefs / se
  pr <- 2*pnorm(-abs(zval))
  
  new("pim.summary",
      formula=formula(object),
      coef = coefs,
      se = se,
      zval = zval,
      pr = pr
  )
}

#' @rdname summary.pim
setMethod('summary',
          signature='pim',
          summary.pim)