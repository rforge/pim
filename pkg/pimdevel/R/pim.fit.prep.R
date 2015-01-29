#' Prepare the data for a pim model
#' 
#' This function is far from finished, but prepares the model matrix
#' for the \code{\link{pim.fit}} function. It has to be redone completely
#' 
#' @inheritParams pim
#' @param ... arguments passed to other methods.
#' 
#' @section Note: The user has in general no need for calling this 
#' function. It is called by \code{\link{pim}}, which is the preferred
#' user interface for fitting a probabilistic index model
#' 
#' @return An object that can serve as the input for pim.fit. TO BE DONE
#' 
#' @export
pim.fit.prep <- function(
  formula,
  data,
  model=c('difference','marginal','custom'),
  compare=c('unique','all','custom'),
  ...
  ){
  
  # Argument checks
  model <- match.arg(model)
  compare <- match.arg(compare)
  nodata <- missing(data)
  
  # Check formula and extract info
  lhs <- formula[[2]]
  rhs <- formula[[3]]
  
  response <- all.vars(lhs)
  predictors <- all.vars(rhs)
  
  funs.rhs <- setdiff(all.names(formula[[3]],unique=TRUE),
                      predictors)
  funs.lhs <- setdiff(all.names(formula[[2]],unique=TRUE),
                      predictors)
  
  has.funs.lhs <- any(match(funs.lhs,.specials.pim.lhs,0L) >0L  )
  has.funs.rhs <- any(match(funs.rhs,.specials.pim.rhs,0L) >0L  )
  
  X <- eval(parse(text=paste("quote(","PO(L(Height),R(Height))",")")))
  

  browser()
  

}
