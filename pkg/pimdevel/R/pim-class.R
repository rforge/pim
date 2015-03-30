#' Class pim
#' 
#' This class contains the fitting information resulting from a call to
#' \code{\link{pim}}. 
#' 
#' @slot formula The \code{\link{pim.formula}} object used in the fit
#' @slot coef a numeric vector with the fitted coefficients
#' @slot vcov a numeric matrix containing the variance-covariance matrix 
#' of the fitted coefficients
#' @slot penv a \code{\link{pim.environment}} object containing the 
#' data used to fit this 
#' @slot fitted a numeric vector containing the raw fitted 
#' @slot link a character vector describing the used link function
#' @slot estimators a list with the elements \code{coef} and \code{vcov},
#' containing either a character value with the name of the used estimator,
#' or the function itself.
#' 
#' @include pim.formula-class.R pim.environment-class.R
setClass(
  'pim',
  slots=c(formula='pim.formula',
          coef = 'numeric',
          vcov = 'matrix',
          penv = 'pim.environment',
          fitted = 'numeric',
          link = 'character',
          estimators = 'list'),
  validity=function(object){
    if(names(object@estimators) != c('coef','vcov')){
      "The list of estimators is malformed"
    } else {
      TRUE
    }
  }
  )