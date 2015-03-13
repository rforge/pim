#' Fitter function for a probabilistic index model
#' 
#' This is the basic computing engine called by \code{\link{pim}}
#' to get the estimates for the coefficients and the variance-
#' covariance matrices. This function currently only spits out 
#' these components using the sandwich estimators. 
#' 
#' NEEDS WORK
#' 
#' @param x a model matrix of dimensions n*p
#' @param y a response vector with n values
#' @param link a character vector with a link function
#' @param estim a character vector or a function indicating the solver
#' to be used for estimating the coefficients. By default this is
#' the function \code{\link[nleqslv]{nleqslv}}. Other possibilities are
#' given in the help page on \code{\link{estimators}}.
#' @param start A numeric vector with the starting values for the fitting
#' algorithm, if required. 
#' 
#' @return In all cases, a list with the coefficients
#' 
#' @export
pim.fit <- function(x,y,link = "probit",
                    estim = 'estimator.nleqslv',
                    start = rep(0,ncol(x)),
                    ...
                    )
{
  estim <- match.fun(estim)
  res <- estim(x, y, link = link, start=start, ...)
  
  vcov <- "not yet implemented"
  
  return(list(coef = res$coef,vcov = vcov))
  
}