#' Extract information from a pim.formula object
#' 
#' This group of functions provides an easy way to extract the
#' extra information saved in a \code{\link{pim.formula}} object. 
#' Take a look at the help page of \code{\link{pim.formula}} for
#' more information.
#' 
#' @include pim.formula-class.R
#' @rdname has.specials
#' @export
#' @return \code{has.specials()}: a single \code{TRUE} or \code{FALSE}
#' value indicating whether the formula right-hand side contains any
#' special functions.
setGeneric("has.specials",
           function(x)standardGeneric("has.specials"))

setMethod("has.specials",
          signature= "pim.formula",
          function(x) x@has.specials)

#' @rdname has.specials
setGeneric("terms")

setMethod("terms",
          signature= "pim.formula",
          function(x) x@terms)

#' @rdname has.specials
setGeneric("lhs",
           function(x)standardGeneric("lhs"))

setMethod("lhs",
          signature= "pim.formula",
          function(x) x@lhs)

#' @rdname has.specials
setGeneric("penv",
           function(x) standardGeneric("penv"))

setMethod("penv",
          signature = "pim.formula",
          function(x) x@penv)