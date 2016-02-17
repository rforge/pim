#' Extract response from a pim.formula
#' 
#' This function extracts the response from a 
#' \code{\link{pim.formula}} for use in \code{\link{pim.fit}}.
#' 
#' @param object an object of class pim or pim.formula.
#' 
#' @return The response variable with pseudo-observations for
#' a pim. 
#' 
#' @seealso \code{\link{pim-class}} and \code{\link{pim.formula-class}}
#' for more information on the classes, and \code{\link{pim}}, 
#' \code{\link{pim.fit}} and \code{\link{pim.formula}} for more
#'  information on related functions.
#' @docType methods
#' @export
setGeneric("response", function(object) standardGeneric("response"))

#' @rdname response
setMethod("response",
          signature = "pim.formula",
          function(object){
            eval(object@lhs, envir=penv(object))
  })

#' @rdname response
setMethod("response",
          signature = "pim",
          function(object){
            if(object@keep.data)
              object@response
            else
              response(object@formula)
          })