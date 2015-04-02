#' Extract response from a pim.formula
#' 
#' This function extracts the response from a 
#' \code{\link{pim.formula}} for use in \code{\link{pim.fit}}.
#' #' 
#' @param formula a \code{\link{pim.formula}} object.
#' 
#' @return The response variable with pseudo-observations for
#' a pim. 
#' 
#' @export
setGeneric("response", function(object) standardGeneric("response"))

setMethod("response",
          signature = "pim.formula",
          function(object){
            eval(object@lhs, env=penv(object))
  })

setMethod("response",
          signature = "pim",
          function(object){
            if(keep.data(object))
              object@response
            else
              response(object@formula)
          })