#' Create a model matrix for a probabilistic index model
#' 
#' This function creates a model matrix for use in a probabilistic
#' index model. This model matrix can be passed to \code{\link{pim.fit}}.
#' 
#' @param object a \code{\link{pim.formula}} object that contains
#' the formula necessary for constructing the model matrix. 
#' @param data an optional argument specifying the data frame for which
#' the model matrix should be constructed. See also 
#' \code{\link[stats]{model.matrix})} in the \code{stats} package.
#' 
#' @param ... extra arguments passed to or from other methods.
#' This is currently only implemented in concordance with
#' the generic \code{\link[stats]{model.matrix}} function.
#' 
#' @return a design matrix for a pim model
#' 
#' @examples 
#' data("FEVData")
#' # Create the "model frame"
#' FEVenv <- new.pim.env(FEVData, compare="unique")
#' # This includes the poset
#' pos <- poset(FEVenv, as.list=TRUE)
#' 
#' # create the formula and bind it to the pim.environment.
#' FEVform <- new.pim.formula(
#'   Age ~ I(L(Height) - R(Height))  ,
#'   FEVenv
#' )
#' 
#' # Use this formula object to construct the model matrix
#' MM <- model.matrix(FEVform)
#' 
#' # Use this formula object to construct the pseudo response
#' Y <- response(FEVform)
#' 
#' # Now pim.fit can do what it does
#' res <- pim.fit(MM,Y, estim = "estimator.glm", penv=FEVenv)
#' 
#' @rdname model.matrix.pim
#' @aliases model.matrix, model.matrix.pim.formula
#' @export
#' @include pim.formula-class.R
setGeneric("model.matrix")

#' @rdname model.matrix.pim
setMethod("model.matrix",
          signature="pim",
          function(object, data, ...){
            if(!missing(data))
              warning("data argument ignored. specifying it when using model.matrix() on a pim object doesn't really make any sense.")
            if(object@keep.data){
              return(object@model.matrix)
            } else {
              model.matrix(object@formula, ...)
            }
          })

# The actual (S3) method to keep functionality flowing even when
# model.matrix is called in other packages.
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

#' @rdname model.matrix.pim
setMethod("model.matrix",
          signature="pim.formula",
          model.matrix.pim.formula
)
