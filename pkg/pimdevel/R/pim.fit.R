#' Fitter function for a probabilistic index model
#' 
#' This is the basic computing engine called by \code{\link{pim}}
#' to get the estimates for the coefficients and the variance-
#' covariance matrices. This function currently only spits out 
#' these components using the sandwich estimators. 
#' 
#' NEEDS WORK
#' 
#' @param x a model matrix with as many rows as \code{y}.
#' @param y a vector with the pseudo-responses
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
#' @seealso \code{\link{model.matrix}} for how to construct a valid model matrix
#' for a pim, \code{\link{pim}} for the general user interface
#' 
#' @export
pim.fit <- function(x,y,link = "logit",
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