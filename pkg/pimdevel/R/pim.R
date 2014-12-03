#' Fitting a Probabilistic Index Model
#' 
#' This function is the main function to fit a probabilistic index model
#' or PIM. 
#' 
#' @param formula An object of class \code{\link{formula}} (or one that
#' can be coerced to that class): A symbolic description of the model
#' to be fitted. The details of model specification are given under 'Details'.
#' 
#' @param data an optional data frame, list or environment that contains
#' the variables in the model. Objects that can be coerced by
#' \code{\link{as.data.frame}} can be used too.
#' 
#' @param link a character vector with a single value that determines the
#' used link function. Possible values are "logit", "probit" and "identity".
#' The default is "identity".
#' 
#' @param compare a character vector with a single value that describes how the 
#' model compares observations. It can take the values "unique" or "all". Alternatively you can pass a matrix with two columns. Each row represents the rownumbers in the original data frame that should be compared to eachother. See Details.
#' 
#' @param model a single character value with possible values "difference" 
#' (the default), "marginal", "regular" or "customized". If the formula indicates
#' a customized model (by the use of \code{\link{L()}} or \code{\link{R()}}),
#' this parameter is set automatically to "customized"
#' 
#' @param na.action a function which indicates what should happen when the data
#' contains NAs. The default is set by thhe \code{na.action} setting of
#' \code{\link{options}}, and is \code{\link{na.fail}} when unset. 
#' 
#' @param vcov a function to determine the variance-covariance matrix. Possibilities are \code{\link{vcov.sandwich}} and \code{link{vcov.score}}
#' 
#' @return An object of class \code{pim}
#' 
