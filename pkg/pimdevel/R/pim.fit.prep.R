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
  
  # Get info on formula
  orig.terms <- terms(formula, specials=c('L','R','I','f')) 
  varnames <- all.vars(orig.terms)
  fenv <- environment(formula)
  
  # Checking data and getting the variables if necessary
  if(missing(data) & 
       all(sapply(varnames,exists,envir=fenv))){
    data <- mget(varnames,
                 envir = fenv,
                 inherits = TRUE)
    nobs <- unique(sapply(data,length))
    browser()
    
    if(length(nobs) != 1)
      stop("Not all variables in the formula have the same length.")
    
    if(any(sapply(data,class)=="character"))
      warning("Character variable(s) converted to factor.")
  } else {
    nobs <- nrow(data)
  }  
  
  data <- list2env(data, parent=fenv)
  pimenv <- new.env(parent=data)
  # pimenv will contain the poset and L() and R() functions
  # to use model.frame etc, we need the data to be an environment
  # and also a parent of this pimenv. Otherwise one will get
  # into trouble with variables called "poset" or "data" when
  # evaluating the formulas. 
  
  
  # Create the poset
  pimenv$poset <- create.poset(compare,nobs)
  
  #Create the left and right functions
  
  browser()
  
  # Rework the formula.
  # TODO: This needs some thinking about names, see Nicks work #TODO
  
  # export all shit.

}
