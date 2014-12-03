#' Add functions to a pim 
#' 
#' The function \code{f()} can be used to add custom functions
#' to the formula interface of the \code{\link{pim}} function. 
#' It allows you to specify custom pims. See the examples and details
#' section for more information on how to use this function.
#' 
#' @param expr an expression that represents the part of the model. It can
#' (and should) contain the indications \code{\link{L}} and \code{\link{R}}
#' to specify the left-hand and right-hand side of the comparisons. If
#' no specifications for either side are found, the expression is calculated
#' and the result treated as a normal variable. In that case \code{f()} is
#' equivalent to \code{\link{\I}}
#' @param data a frame where the expression can be evaluated. This parameter
#' should not be set by the user, but by the function calling f(). It's 
#' either the parent frame of that function (default) or it should be
#' set to be the data frame in which the variables can be found.
#' 
#' @return The value of expr as an \code{\link{expression}}

f <- function(expr,data=parent.frame(n=2)){
  # Extract the expression
  expr <- as.expression(match.call()$expr)
  
  structure(expr, class = unique(c("pim.expression", oldClass(expr))))
}
