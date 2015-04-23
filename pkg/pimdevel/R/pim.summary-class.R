#' Class pim.summary
#' 
#' This class contains the summary information from a probabilistic index
#' model , and is created by using the function \code{\link{summary}}
#' on an object of the \code{\link{pim-class}}.
#' 
#' TO BE FINISHED
#' 
#' @seealso \code{\link{pim}} for more info on how to construct the model
#' 
#' @include pim-class.R
#' 
setClass(
  'pim.summary',
  slots=c(formula='pim.formula',
          coef = 'numeric',
          vcov = 'matrix')
)

print.pim.summary <- function(x, digits = max(3L, getOption("digits") - 3L),...){
  NULL
}

