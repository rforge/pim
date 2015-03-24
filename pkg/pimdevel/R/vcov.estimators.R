#' vcov estimators for pim
#' 
#' \code{vcov.sandwich} and \code{vcov.score} are two similar estimators
#' for the variance-covariance matrix of a probabilistic index model.
#' These functions are meant to be used within a call to \code{\link{pim}}
#' as a value for the argument \code{vcov}.
#' 
#' @note You should only use \code{vcov.score} in combination with an 
#' identity link
#' 
#' @param Z the design matrix
#' @param beta a numeric vector with the fitted coefficients
#' @param Y a numeric vector with pseudoresponses
#' @param W a numeric vector with weights. If weights are not applicable,
#' set to \code{NULL} (the default)
#' @param ... arguments passed to downstream methods.
#' 
#' @return the variance-covariance matrix
#' 
#' @rdname vcov.estimators
#' @name vcov.estimators
#' @aliases vcov.sandwich vcov.score
#' 
vcov.sandwich <- function(Z,beta,Y,W,...){
  NULL
}