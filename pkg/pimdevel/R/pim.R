#' Fitting a Probabilistic Index Model
#' 
#' This function is the main function to fit a probabilistic index model
#' or PIM. 
#' 
#' @section WARNING: THIS FUNCTION CURRENTLY ONLY RETURNS A MODEL MATRIX
#' 
#' @param formula An object of class \code{\link{formula}} (or one that
#' can be coerced to that class): A symbolic description of the model
#' to be fitted. The details of model specification are given under 'Details'.
#' 
#' @param data an optional data frame, list or environment that contains
#' the variables in the model. Objects that can be coerced by
#' \code{\link{as.data.frame}} can be used too.
#' 
#' @param link a character vector with a single value that determines the
#' used link function. Possible values are "logit", "probit" and "identity".
#' The default is "identity".
#' 
#' @param compare a character vector with a single value that describes how the 
#' model compares observations. It can take the values "unique" or "all". Alternatively you can pass a matrix with two columns. Each row represents the rownumbers in the original data frame that should be compared to eachother. See Details.
#' 
#' @param model a single character value with possible values "difference" 
#' (the default), "marginal", "regular" or "customized". If the formula indicates
#' a customized model (by the use of \code{\link{L}()} or \code{\link{R}()}),
#' this parameter is set automatically to "customized"
#' 
#' @param na.action a function which indicates what should happen when the data
#' contains NAs. The default is set by the \code{na.action} setting of
#' \code{\link{options}}, and is \code{\link{na.fail}} when unset. 
#' 
#' @param vcov a function to determine the variance-covariance matrix. Possibilities are \code{\link{vcov.sandwich}} and \code{link{vcov.score}}.
#' Defaults to \code{vcov.sandwich} --- NOT IMPLEMENTED YET ---
#' 
#' @param ... extra parameters sent to \code{\link{pim.fit}}
#' 
#' @return An object of class \code{pim}
#' 
#' @section Note:
#' THIS FUNCTION DOESN'T DO ANYTHING FOR THE MOMENT. WORK NEEDED.
#' 
#' @export
pim <- function(formula,
                data,
                link = c("identity","logit","probit"),
                compare = c("unique","all"),
                model = c("difference","marginal",
                          "regular","customized"),
                na.action = getOption("na.action"),
                vcov = 'vcov.sandwich',
                ...
                ){
  
  # Check the arguments
  model <- match.arg(model)
  compare <- match.arg(compare)
  link <- match.arg(link)
  nodata <- missing(data)
  #vcov <- match.fun(vcov)
  
  if(is.null(na.action)) na.action <- "na.fail"
  
  # Check formula and extract info
  f.terms <- terms(formula, simplify=TRUE)
    
  vars <- all.vars(formula)
  
  if(nodata){
    if(!all(pres <- vars %in% ls(parent.frame())) )
      stop(paste("Following variables can't be found:",
                 .lpaste(vars[!pres]))
           )
  } else {
    if(!all(pres <- vars %in% names(data)))
      stop(paste("Following variables can't be found:",
                 .lpaste(vars[!pres]))
           )
           
  }
  
  # Create the pim environment (similar to model frame)
  
  penv <- if(nodata) 
    new.pim.env(parent.frame(),compare = compare, vars=vars)
  else
    new.pim.env(data, compare = compare, vars=vars)
  
  ff <- new.pim.formula(formula, penv)
  
  x <- model.matrix(ff)
  y <- eval(lhs(ff), env=penv)
  
  res <- pim.fit(x, y, link, ...)
  
  res
}