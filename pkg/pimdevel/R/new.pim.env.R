#' Constructor for a pim.environment
#' 
#' This functions serves as a constructor for an object of the class
#' \code{\link{pim.environment}}. 
#' 
#' @param data a data frame, a list or an environment containing
#' the data for a probabilistic index model. 
#' @param poset either a logical value indicating whether or not a poset
#' should be added, or list with 2 elements that give the 
#' left- and right hand side of the poset. See also \code{\link{create.poset}}
#' @param env an environment that is the parent environment of the object.
#' @param ... extra parameters for construction of the poset and
#' specification of the parent environment of the object. See also
#' \code{\link{parent.env()}}
#' 
#' @return an object of the class 
setGeneric("new.pim.env",
           function(data,poset=FALSE,...){
             standardGeneric("new.pim.env")
           })

setMethod("new.pim.env",
          signature=c(data="missing",
                      poset="ANY"),
          function(data,poset,...){
            new("pim.environment")
          })

setMethod("new.pim.env",
          signature=c(data="environment",
                      poset="ANY"),
          function(data,poset=FALSE,
                   data.names=ls(data),
                   classes=.get.classes(data),...){
            
            out <- new("pim.environment")
            out@.xData <- data
            out@data.names <- data.names
            out@classes <- classes
            out@nobs <- length(get(data.names[1],envir=data,inherits=FALSE))
            validObject(out)
            out
          })

setMethod("new.pim.env",
          signature=c(data="list",
                      poset="ANY"),
          function(data,poset=FALSE,...){
            data.names <- names(data)
            classes <- sapply(data,class, simplify=FALSE)
            
            if(!valid.classes(classes)){
              stop("Some list elements are of a wrong class.")
            }
            
            if(length(nobs <- unique(sapply(data,length)))!=1){
              stop("All elements in the list should have the same length")
            }
            browser()
            .new.pim.env(data,
                         poset,
                         data.names=data.names,
                         classes=classes,
                         ...)
          })

setMethod("new.pim.env",
          signature=c(data="data.frame",
                      poset="ANY"),
          function(data,poset,...){
            .new.pim.env(data,poset,...)
          })

# The function .new.pim.env : the actual workhorse.

.new.pim.env <- function(data,poset=FALSE,env=parent.frame(),
                         data.names,
                         classes,
                         nobs=NULL,
                         compare=c('unique','all','custom'),
                         ...){
  object <- new("pim.environment")
  
  if(!(is.list(poset) || is.logical(poset)))
    stop("poset should be logical or a list.")
  print(poset)
  
  if(is.list(poset)){
    
    if(!"custom" %in% compare)
      warning("custom poset specified. Argument compare ignored.")
    
  } else if(poset){
    
  }
  
}
