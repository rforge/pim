#' Constructor for a pim.environment
#' 
#' This functions serves as a constructor for an object of the class
#' \code{\link{pim.environment}}. In most cases, calling this function directly
#' is not necessary.
#' 
#' @details This function is called during the preparation of the model
#' matrix for a pim. The resulting object is used to evaluate the formula
#' of a pim, and stores information on how this is done. 
#' 
#' TODO : Currently there's no automatic assignment of a parent frame, but this
#' will be added in the near future.
#' 
#' @param data a data frame, a list or an environment containing
#' the data for a probabilistic index model. 
#' @param poset either a logical value indicating whether or not a poset
#' should be added, or otherwise a matrix or list with 2 elements that give the 
#' left- and right hand side of the poset. See also \code{\link{new.pim.poset}} 
#' for more information on how to specify a custom poset
#' @param env an environment that is the parent environment of the object.
#' @param data.names An optional character vector with the names of the variables
#' in the pim environment.
#' @param classes An optional character vector with the classes of the 
#' variables in the environment, given in the same order as 
#' the argument \code{data.names}.
#' @param ... extra parameters for construction of the poset, like
#' the argument \code{compare} from \code{\link{new.pim.poset}}.
#' 
#' @return an object of the class \code{\link{pim.environment}}
#' @include pim.environment-class.R
#' @aliases new.pim.env
#' @examples
#' new.pim.env() # Creates an empty object
#' 
#' # Starting from a data frame
#' data(DysData)
#' env1 <- new.pim.env(DysData)
#' 
#' env2 <- new.pim.env(DysData, poset=TRUE)
#' env3 <- new.pim.env(DysData, poset=TRUE, compare="all")
#' 
#' 
#' 
#' @export
setGeneric("new.pim.env",
           function(data,poset=FALSE,...){
             standardGeneric("new.pim.env")
           })
#' @describeIn new.pim.env
setMethod("new.pim.env",
          signature=c(data="missing",
                      poset="ANY"),
          function(data,poset){
            if(!is.logical(poset))
              stop("data not specified.")
            if(poset) 
              stop("data not specified, so poset cannot be constructed.")
            new("pim.environment")
          })

#' @describeIn new.pim.env
setMethod("new.pim.env",
          signature=c(data="missing",
                      poset="missing"),
          function(data,poset,...){
            new("pim.environment")
          })


#' @describeIn new.pim.env
setMethod("new.pim.env",
          signature=c(data="environment",
                      poset="ANY"),
          function(data,poset=FALSE,
                   env=parent.frame(),
                   data.names=ls(data),
                   classes=.get.classes(data),...){
            
            dots <- match.call(expand.dots=FALSE)[['...']]
            if(match('nobs',names(dots),0L) >0L)
              warning('nobs argument is ignored.')
            
            out <- new("pim.environment")
            out@.xData <- data
            out@data.names <- data.names
            out@classes <- classes
            out@nobs <- length(get(data.names[1],envir=data,inherits=FALSE))
            
            # create poset
            if(is.logical(poset)){
              if(poset)
                out@poset <- new.pim.poset(nobs=out@nobs,...)
              out@is.complete <- TRUE
            } else{
              out@poset <- new.pim.poset(poset)
              out@is.complete <- TRUE
            }
            
            validObject(out)
            parent.env(out) <- env
            out
          })

#' @describeIn new.pim.env
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
            .new.pim.env(data,
                         poset,
                         data.names=data.names,
                         classes=classes,
                         nobs=nobs,
                         ...)
          })

#' @describeIn new.pim.env
setMethod("new.pim.env",
          signature=c(data="data.frame",
                      poset="ANY"),
          function(data,poset=FALSE,...){
            .new.pim.env(data,
                         poset,
                         data.names=names(data),
                         nobs=nrow(data),
                         ...)
          })

# The function .new.pim.env : the actual workhorse.

.new.pim.env <- function(data,poset=FALSE,env=parent.frame(),
                         data.names,
                         classes,
                         nobs,
                         compare=c('unique','all','custom'),
                         ...){
  object <- new("pim.environment")
  
  # check input
  compare <- match.arg(compare)
  
  if(missing(classes))
    classes <- sapply(data,class,simplify=FALSE)
  
  if(missing(data.names))
    data.names <- names(data)
  
  if(!(is.list(poset) || is.logical(poset)))
    stop("poset should be logical or a list.")
  
  # Add information
  object@.xData <- as.environment(data)
  object@data.names <- data.names
  object@classes <- classes
  object@nobs <- nobs
    
  if(!is.logical(poset) & compare != "custom"){
    
    if(compare != "custom")
      warning("custom poset specified. Argument compare ignored.")
    
    object@poset <- new.pim.poset(poset,nobs)
    object@is.complete <- TRUE
    
  } else if(poset){
    if(compare == "custom")
      stop("custom poset not applied as list")
    object@poset <- new.pim.poset(compare,nobs)
    object@is.complete <- TRUE
  }

  validObject(object)
  parent.env(object) <- env
  object
  
}
