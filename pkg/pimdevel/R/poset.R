#' Extract the poset as a matrix or list
#' 
#' This function allows you to extract the poset from either a 
#' \code{\link{pim.environment}} or a \code{\link{pim.poset}} object.
#' The poset can be extracted as a matrix or a list. 
#' 
#' @param x an object of class \code{\link{pim.environment}} 
#' or \code{\link{pim.poset}}
#' 
#' @param as.list a logical value indicating whether the poset should
#' be returned as list or as a matrix. Defaults to FALSE, which returns
#' a matrix
#' 
#' @param ... arguments passed to other methods. Currently ignored.
#' 
#' @return When x contains a poset, either a matrix or a list 
#' (when \code{as.list} is \code{TRUE}) with the indices that
#' make up the poset. If there's no poset, the function returns
#' a missing value.
#' 
#' The returned matrix hax 2 columns, each named after the 
#' respective poset function. In case a list is requested, the function
#' returns a named list with 2 elements, each element containing the
#' indices related to the poset function of the same name
#' 
#' @examples
#' data(DysData)
#' DysPimEnv <- new.pim.env(DysData, poset=TRUE)
#' poset(DysPimEnv)
#' 
#' @export
setGeneric('poset', function(x,...) standardGeneric('poset'))

#' @describeIn poset
#' @export
setMethod('poset',
          signature='pim.environment',
          function(x,
                   ...){
            if(is.complete(x)){
              poset(x@poset,...)
            } else{
              NA
            }
          })

#' @describeIn poset
#' @export
setMethod('poset',
          signature='pim.poset',
          function(x,
                   as.list=FALSE){
            obj <- ls(x)
            if(identical(character(0),obj)){
              return(NA)
            } else {
              out <- lapply(obj,
                            function(i) environment(x[[i]])$poset
                            )
              names(out) <- obj
              if(as.list){
                return(out)
              } else {
                return(do.call(cbind,out))
              }
            }
          })