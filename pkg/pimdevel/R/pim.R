#' Fitting a Probabilistic Index Model
#' 
#' This function is the main function to fit a probabilistic index model
#' or PIM. 
#' 
#' A BIT MORE INFO ON FITTING MODELS IS NEEDED
#' 
#' It's possible to store the model matrix and psuedo responses in the
#' resulting object. By default this is not done 
#' (\code{keep.data = FALSE}) as this is less burden on the memory and
#' the \code{\link{pim.formula}} object contains all information to
#' reconstruct both the model matrix and the pseudo responses. 
#' If either the model matrix or the pseudo responses are needed for
#' further calculations, setting \code{keep.data} to \code{TRUE} might
#' reduce calculation time for these further calculations.
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
#' The default is "logit".
#' 
#' @param compare a character vector with a single value that describes how the 
#' model compares observations. It can take the values "unique" or "all". Alternatively you can pass a matrix with two columns. Each row represents the rownumbers in the original data frame that should be compared to eachother. See Details.
#' 
#' @param model a single character value with possible values "difference" 
#' (the default), "marginal", "regular" or "customized". If the formula indicates
#' a customized model (by the use of \code{\link{L}()} or \code{\link{R}()}),
#' this parameter is set automatically to "customized"
#' 
#' @param na.action the name of a function which indicates what should happen when the data
#' contains NAs. The default is set by the \code{na.action} setting of
#' \code{\link{options}}, and is \code{\link{na.fail}} when unset.
#' 
#' @param keep.data a logical value indicating whether the model
#' matrix should be saved in the object. Defaults to \code{FALSE}. See Details. 
#' 
#' @param ... extra parameters sent to \code{\link{pim.fit}}
#' 
#' @return An object of class \code{pim}. See \code{\link{pim-class}}
#' for more information.
#' 
#' @seealso \code{\link{pim-class}} for more information on the returned
#' object, \code{\link{pim.fit}} for more information on the fitting
#' itself, and --- INSERT GETTERS ---
#' 
#' 
#' @export
pim <- function(formula,
                data,
                link = c("logit","probit","identity"),
                compare = c("unique","all"),
                model = c("difference","marginal",
                          "regular","customized"),
                na.action = getOption("na.action"),
                weights=NULL,
                keep.data = FALSE,
                ...
                ){
  
  # Check the arguments
  model <- match.arg(model)
  compare <- match.arg(compare)
  nodata <- missing(data)
  #vcov <- match.fun(vcov)
  link <- match.arg(link)
  
  if(is.null(na.action)) na.action <- "na.fail"
  if(!is.character(na.action)) 
    na.action <- deparse(substitute(na.action))
  
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
  
  x <- model.matrix(ff, na.action = na.action)
  y <- eval(lhs(ff), envir=penv)
  
  res <- pim.fit(x, y, link, weights = weights, 
                 penv = as.environment(penv@poset), ...)
  # as.environment will only pass the environment of the penv to avoid
  # copying the whole thing. makes it easier to get the poset out
  
  names(res$coef) <- colnames(x)
  
  if(!keep.data){
    x <- matrix(nrow=0,ncol=0)
    y <- numeric(0)
  } 
  
  new.pim(
    formula = ff,
    coef = res$coef,
    vcov = res$vcov,
    fitted = res$fitted,
    penv = penv,
    link = link,
    estimators=res$estim,
    model.matrix = x,
    na.action = na.action,
    response = y,
    keep.data = keep.data)
}