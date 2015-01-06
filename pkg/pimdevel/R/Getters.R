#' Extract information from pim.environment and pim.poset objects
#' 
#' These functions serve to extract the information contained
#' in the objects of class \code{\link{pim.environment}} and 
#' \code{\link{pim.poset}}. 
#' 
#' @param x an object of class \code{pim.environment} or \code{pim.poset}
#' 
#' @return \code{classes()}: A named vector with the classes of the data 
#' contained in the \code{pim.environment}
#' 
#' @seealso \code{\link{nobs}}, \code{\link{poset}}, \code{\link{is.complete}}
#' 
#' @examples
#' data(DysData)
#' DysPimEnv <- new.pim.env(DysData,poset=TRUE)
#' classes(DysPimEnv)
#' names(DysPimEnv)
#' compare(DysPimEnv)
#' 
#' @aliases data.names, compare
#' @include pim.environment-class.R
#' @export
setGeneric('classes', function(x) standardGeneric('classes'))

#' @export
#' @rdname classes
setMethod('classes',
          signature='pim.environment',
          function(x){
            unlist(x@classes)
          })

## NOTE:
# names is a primitive function, hence a S4 generic
# is already available in the base package. Creating
# a generic in the package here only results in warnings.
# The generic won't be created, so when trying to export,
# there's nothing to export.

#' @return \code{names()}: For an object of class \code{pim.environment} the names
#' of the variables in the object. For an object of class \code{pim.poset},
#' the name of the poset functions inside the environment
#' @export
#' @rdname classes
setMethod('names',
          signature='pim.environment',
          function(x){
            x@data.names
          })

#' @export
#' @rdname classes
setMethod('names',
          signature='pim.poset',
          function(x){
            ls(x)
          })

#' @export
#' @rdname classes
#' @return \code{compare()}: A character value indicating how the comparison
#' is defined in a \code{pim.poset} object, or the poset-slot of 
#' a \code{pim.environment} object respectively.
setGeneric('compare',function(x) standardGeneric('compare'))

#' @export
#' @rdname classes
setMethod('compare',
          signature=c('pim.environment'),
          function(x){
            x@poset@compare
          })

#' @export
#' @rdname classes
setMethod('compare',
          signature=c('pim.poset'),
          function(x){
            x@compare
          })
