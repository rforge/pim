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
#' 
#' @export
#' @import nleqslv
pim.fit <- function(x,y,link,...
                    )
  {
  fn <- CreateScoreFun(x,y,link)
  startvalues <- rep(0,ncol(x))
  nleqslv(startvalues, fn)
}