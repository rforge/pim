#' Create a model matrix for a probabilistic index model
#' 
#' This function creates a model matrix for use in a probabilistic
#' index model. This model matrix can be passed to \code{\link{pim.fit}}
#' 
#' @param object a \code{\link{pim.formula}} object that contains
#' the formula necessary for constructing the model matrix. 
#' @param data an optional argument specifying the data frame for which
#' the model matrix should be constructed. See also 
#' \code{\link[stats]{model.matrix})} in the \code{stats} package.
#' 
#' @export
#' @include pim.formula-class.R
setGeneric("model.matrix")

setMethod("model.matrix",
          signature="pim",
          function(object, ...){
            if(object@keep.data){
              return(object@model.matrix)
            } else {
              model.matrix(object@formula, ...)
            }
          })

# The actual (S3) method to keep functionality flowing even when
# model.matrix is called in other packages.
#' @export
model.matrix.pim.formula <-
  function(object, data, ...){
    if(missing(data)) data <- object@penv
    
    tt <- terms(object)
    specials <- has.specials(object)
    
    if(specials){
      tt[[2]] <- object@lhs
      tt <- terms(formula(tt, env=data))
      
    }
    mm <- model.matrix(tt,
                       data, ...)
    
    if(!specials){
      pos <- poset(data, as.list=TRUE)
      mm <- mm[pos$R,] - mm[pos$L,]
    }
    
    if(has.intercept(object)){
      if((id <- match("(Intercept)",colnames(mm),0L)) > 0L){
        mm[,id] <- 1
      } else {
        mm <- cbind(mm, "(Intercept)" = 1)
      }
    } else{
      if((id <- match("(Intercept)",colnames(mm),0L)) > 0L){
        mm <- mm[,-id, drop=FALSE]
      }
    }
    
    mm
  }

setMethod("model.matrix",
          signature="pim.formula",
          model.matrix.pim.formula
)