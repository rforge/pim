#' Extract information from a pim.formula object
#' 
#' This group of functions provides an easy way to extract the
#' extra information saved in a \code{\link{pim.formula}} object. 
#' Take a look at the help page of \code{\link{pim.formula}} for
#' more information.
#' 
#' @param x an object of the class pim.formula
#' @param ... arguments passed to other methods
#' 
#' @seealso the class \code{\link{pim.formula-class}}
#' 
#' @examples 
#' data("FEVData")
#' # Create the "model frame"
#' FEVenv <- new.pim.env(FEVData, compare="unique")
#' 
#' # create the formula and bind it to the pim.environment.
#' FEVform <- new.pim.formula(
#'   Age ~ I(L(Height) - R(Height))  ,
#'   FEVenv
#' )
#' lhs(FEVform)
#' has.specials(FEVform)
#' penv(FEVform)
#' 
#' FEVform2 <- new.pim.formula(
#'   FEV ~ Height*Sex,
#'   FEVenv
#' )
#' 
#' has.specials(FEVform2)
#' terms(FEVform2)
#' 
#' @include pim.formula-class.R
#' @rdname has.specials
#' @aliases lhs, terms, penv
#' @export
#' @return \code{has.specials()}: a single \code{TRUE} or \code{FALSE}
#' value indicating whether the formula right-hand side contains any
#' special functions.
setGeneric("has.specials",
           function(x)standardGeneric("has.specials"))

#' @rdname has.specials
setMethod("has.specials",
          signature= "pim.formula",
          function(x) x@has.specials)

#' @rdname has.specials
#' @return \code{terms()}: the \code{\link[stats]{terms}} object
#' of the \code{pim.formula} object
#' @export
setGeneric("terms")

#' @rdname has.specials
setMethod("terms",
          signature= "pim.formula",
          function(x) x@terms)

#' @rdname has.specials
#' @export
#' @return \code{lhs()}: an object of class \code{call} containing 
#' the left hand side of the formula as used in the pim. 
setGeneric("lhs",
           function(x)standardGeneric("lhs"))

#' @rdname has.specials
setMethod("lhs",
          signature= "pim.formula",
          function(x) x@lhs)

#' @rdname has.specials
#' @export
setGeneric("penv",
           function(x) standardGeneric("penv"))

#' @rdname has.specials
setMethod("penv",
          signature = "pim.formula",
          function(x) x@penv)