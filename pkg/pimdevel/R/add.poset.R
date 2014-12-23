#' Add a poset to a pim.environment object
#' 
#' This function adds a poset to a \code{\link{pim.environment}}
#' object.
#' 
#' @param x a pim.environment object
#' @param overwrite a logical value indicating whether the poset
#' should be overwritten if it's already present. Defaults to 
#' \code{FALSE} to avoid problems.
#' @param ... further parameters passed to \code{\link{new.pim.poset}}.
#' Note that you have to provide at least a value for the argument 
#' \code{nobs}. 
#' 
#' @return The object with a (new) poset attached.
#' @seealso \code{\link{new.pim.poset}} for the possible values of the
#' arguments \code{compare} and \code{nobs}.
#' 
#' @include pim.environment-class.R
#' @export 
setGeneric('add.poset',function(x,...) standardGeneric('add.poset'))

#' @describeIn add.poset
setMethod('add.poset',
          signature=c('pim.environment'),
          function(x,overwrite=FALSE,...){
            if(is.complete(x) & !overwrite){
              stop('pim.environment has already a poset attached. Set overwrite to FALSE if you want to replace the current one.')
            }
            x@poset <- new.pim.poset()
          })