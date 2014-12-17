#' Specify the left hand and right hand side of an expression used in pims
#' 
#' These functions allow you to specify the left hand side and 
#' right hand side of a term in a pim model. The user should
#' only use this functions within a formula using the \code{\link{pim}}
#' function. Use in a different context will return an error.
#' 
#' @param x any vector specified in a formula
#' 
#' @return a vector with the pseudo-observations for x, based on the 
#' poset used to create the function
#' 
#' @details These specific functions are actually not used by the 
#' function \code{\link{pim}}. \code{pim} calls the internal function
#' \code{\link{.make.posfun}} to create the actual functions \code{L}
#' and \code{R} to work with the specified posets of the model of 
#' interest.
#' 
#' The actual functions used by \code{pim} are saved in a specific environment, a \code{\link{pim.environment}}, which resides in the \code{\link{pim-class}} object returned by \code{pim}. This way of working
#' is chosen in order to avoid unnecessary copying of data.
#' 
#' @section warning:
#' 
#' These functions serve only as placeholder. During the fitting process
#' of a pim, they get updated to include the posets (the indices that
#' determine which observations are compared)
#' Note that this makes the functions behave fundamentally
#' different from what you would expect R. The result of these
#' functions depends on the context in which they are called.
#' 
L <- function(x){
  stop("L() is not correctly defined. Please see ?L for more information.")  
}

R <- function(x){
  stop("R() is not correctly defined. Please see ?R for more information.")  
}
