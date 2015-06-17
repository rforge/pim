#' Check whether formula has an explicit intercept
#' 
#' This function checks whether an intercept is present in a formula of
#' some form. It works for a \code{\link[stats]{formula}}, a 
#' \code{\link[stats]{terms.object}} a \code{\link{pim.formula}} object or 
#' a character vector representing a formula.
#' 
#' In case of a \code{terms.object}, this function only checks whether the
#' \code{intercept} attribute is larger than 0. In all other cases, the 
#' function checks whether it can find a \code{+ 1} somewhere in the formula,
#' indicating that an intercept has to be fit in a \code{\link{pim}}.
#' 
#' @note This function is meant to be used in the context of a \code{\link{pim}}
#' call. Although the function should work for standard formulas as well,
#' correct results are not guaranteed when used outside a pim context.
#' 
#' @param x either a \code{formula}, \code{pim.formula}, \code{terms.object}
#' or a character vector representing a formula.
#' 
#' @return a single logical value
#' 
#' @include pim.formula-class.R
#' @rdname has.intercept
#' @export
setGeneric('has.intercept', function(x) standardGeneric('has.intercept'))

#' @rdname has.intercept
setMethod('has.intercept',
          signature = 'character',
          function(x){
            grepl("\\+\\s*1\\s*(\\+|$)", remove.pars(x))
          })

#' @rdname has.intercept
setMethod('has.intercept',
          signature='formula',
          function(x){
            has.intercept(as.character(x)[3])
          })

#' @rdname has.intercept
setMethod('has.intercept',
          signature='terms',
          function(x){
            has.intercept(as.character(x)[3])
          })

#' @rdname has.intercept
setMethod('has.intercept',
          signature='pim.formula',
          function(x){
            x@has.intercept
          })