#' Class pim.summary
#' 
#' This class contains the summary information from a probabilistic index
#' model , and is created by using the function \code{\link{summary}}
#' on an object of the \code{\link{pim-class}}.
#' 
#' The class \code{pim.summary} can be treated like a matrix to get out
#' the coefficients, standard errors, Z values and/or p values. 
#' 
#' @slot formula contains an object of the class \code{\link{pim.formula}}
#' containing the model fitted.
#' 
#' @slot coef a numeric vector with the coefficients
#' @slot se a numeric vector with the standard errors for the coefficients
#' @slot zval a numeric vector containing the Z values for the coefficients,
#' testing whether the coefficient differs significantly from 0.
#' @slot pr a numeric vector containing the related p-values for the
#' coefficients.
#' 
#' @seealso \code{\link{pim}} for more info on how to construct the model
#' 
#' @include pim-class.R
#' 
setClass(
  'pim.summary',
  slots=c(formula='pim.formula',
          coef = 'numeric',
          se = 'numeric',
          zval = 'numeric',
          pr = 'numeric'
          ),
  validity = function(object){
    if(!.equal.lengths(
      length(object@coef),
      length(object@se),
      length(object@zval),
      length(object@pr)
    )) stop("coef, se, zval and pr should be of equal length")
     else
       TRUE
  }
)


print.pim.summary <- function(x, digits = max(3L, getOption("digits") - 3L),...){
  cat("pim.summary of following model : \n\n")
  print(formula(x@formula), showEnv = FALSE)
  
  Tab <- cbind(
    Estimate = coef(x),
    "Std. Error" = x@se,
    "Z value" = x@zval,
    "Pr(<|z|)" = x@pr
  )
  cat("\n")
  print(Tab)
}

setMethod("show",
          "pim.summary",
          function(object){
            print.pim.summary(object)
          })