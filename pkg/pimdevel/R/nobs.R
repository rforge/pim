#' Extract the number of observations 
#' 
#' This function extracts the number of observations in an object
#' of class \code{\link{pim.environment}}, or the number of observations
#' for which a \code{\link{pim.poset}} is constructed
#' 
#' @param object an object of the class \code{\link{pim.environment}} or \code{\link{pim.poset}}
#' @param ... arguments passed to other methods.
#' 
#' @return an integer with the information in the nobs slot. If the
#' environment is empty, it returns \code{NA}
#' 
#' @include pim.environment-class.R


#' @describeIn nobs
#' @export
setMethod("nobs",
          signature="pim.environment",
          function(object){
            if(identical(object@nobs,integer(0))){
              NA
            } else {
              object@nobs
            }
          })

#' @describeIn nobs
#' @export
setMethod("nobs",
          signature="pim.poset",
          function(object){
            object@nobs
          }
          
          )
