#' Evaluate pim expressions
#' 
#' This internal function evaluates pim expressions that contain
#' calls to \code{\link{L}} and \code{\link{R}}. It constructs the
#' necessary variables for the \code{\link{pim.fit}} function.
#' 
#' @param expr The pim expression that needs to be evaluated
#' @param frame the frame in which one has to look for the variables
#' in the expression.
#' @param poset a matrix with two columns specifying the indices for
#' the observations. The columns can be named 'L' and 'R', indicating
#' the indices related to the left hand and right hand side respectively.
#' In all other cases, column names are ignored and the first column
#' is taken to be related to the left hand side.
#' 
.eval.pim.expression <-
  function(expr, 
           frame = parent.frame(n=2), 
           poset){
    expr <- match.call()$expr
    browser()
    if(!is(expr,'expression')) 
      stop('expr needs to be of class expression and not a call.
           Try wrapping everything in expression()')
    
  }
