#' Extract the number of observations 
#' 
#' This function extracts the number of observations in an object
#' of class \code{\link{pim.environment}}, or the number of observations
#' for which a \code{\link{pim.poset}} is constructed
#' 
#' @param x an object of the class \code{\link{pim.environment}} or \code{\link{pim.poset}}
#' 
#' @return an integer with the information in the nobs slot. If the
#' environment is empty, it returns \code{NA}
#' 
#' @include pim.environment-class.R
#' 
setGeneric("nobs",function(x) standardGeneric("nobs"))

setMethod("nobs",
          signature="pim.environment",
          function(x){
            if(identical(x@nobs,integer(0))){
              NA
            } else {
              x@nobs
            }
          })

setMethod("nobs",
          signature="pim.poset",
          function(x){
            x@nobs
          }
          
          )
