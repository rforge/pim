#' The summary function for the pim class
#' 
#' The function \code{\link[base]{summary}} is a generic function. We provide
#' a method for objects of the \code{\link{pim-class}}. 
#' 
#' TO BE TESTED
#' 
#' @export
setGeneric('summary')

setMethod('summary',
          signature='pim',
          function(object, ...){
            NULL
          })