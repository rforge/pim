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
    
    if(has.specials(object)){
      tt[[2]] <- object@lhs
      tt <- as.formula(tt, env=data)
    } 

    mm <- model.matrix(tt,
                       data)
    # HEEEEEEEEEEEERRRRRRRRRRRRREEEEEEEEEEEEEEEEEEEEEEEEEEEE
    mm
  }