#' Extract a pim environment from a model or formula
#' 
#' This function allows you to extract the 
#' \code{\link{pim.environment}} object from either a \code{pim}
#' object or a \code{pim.formula} object. 
#' 
#' @param x either a \code{pim} or a \code{pim.formula} object
#' 
#' @return In case of a \code{pim} object, the \code{pim.environment}
#' contained therein. In case of a \code{pim.formula} object, 
#' the environment itself. 
#' See the help page \code{pim.formula-class}.
#' 
#' @export
setGeneric("penv",
           function(x) standardGeneric("penv"))

#' @rdname penv
setMethod("penv",
          signature = "pim.formula",
          function(x) x@penv)

#' @rdname penv
setMethod("penv",
          signature = "pim",
          function(x) x@penv)
