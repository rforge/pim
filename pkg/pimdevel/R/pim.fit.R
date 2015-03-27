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
#' @param vcov.estim a function to determine the variance-covariance matrix. 
#' Possibilities are \code{\link{vcov.sandwich}} and \code{link{vcov.score}}.
#' Defaults to \code{vcov.sandwich}
#' @param penv An environment, \code{\link{pim.environment}} or 
#' \code{\link{pim.poset}} object containing the poset functions.
#' Alternatively this can be a list with two objects, containing the
#' poset indices for the left and right side of the pim.
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
                    vcov.estim = 'vcov.sandwich',
                    weights = NULL,
                    penv,
                    ...
                    )
{
  estim <- match.fun(estim)
  vcov.estim <- match.fun(vcov.estim)
  res <- estim(x, y, link = link, start=start, ...)
  
  fits <- x %*% res$coef

  if(!is.list(penv)) penv <- poset(penv, as.list=TRUE)
  
  vc <- vcov.estim(fits, x, y, weights, link, penv)
  
  return(list(coef = res$coef,vcov = vc))
  
}