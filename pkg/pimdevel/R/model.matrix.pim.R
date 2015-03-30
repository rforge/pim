#' Create a model matrix for a probabilistic index model
#' 
#' This function creates a model matrix for use in a probabilistic
#' index model. This model matrix can be passed to \code{\link{pim.fit}}
#' 
#' @param object a \code{\link{pim.formula}} object that contains
#' the formula necessary for constructing the model matrix. 
#' 
#' @section TO DO:
#' - Finish function
#' 
#' - Create method for pim object for extracting model matrix
#' 
#' @export
#' @include pim.formula-class.R
setGeneric("model.matrix")

setMethod("model.matrix",
          signature="pim.formula",
          function(object, data, ...){
            model.matrix.pim.formula(object, data, ...)
          })

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
                       data)
    
    if (!specials){
      pos <- poset(data, as.list=TRUE)
      mm <- mm[pos$R,] - mm[pos$L,]
    }
    
    if(has.intercept(object)){
      if(id <- match("(Intercept)",colnames(mm),0L) > 0L){
        mm[,id] <- 1
      } else {
        mm <- cbind(mm, "(Intercept)" = 1)
      }
    } else{
      if(id <- match("(Intercept)",colnames(mm),0L) > 0L){
        mm <- mm[,-id, drop=FALSE]
      }
    }
    
    mm
  }