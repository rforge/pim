#' Getters for a pim object
#' 
#' This function allows you to extract a formula from a \code{\link{pim}}
#' or a \code{\link{pim.formula}} object. In the latter case, you extract
#' the original formula.
#' 
#' @param x a \code{pim} or \code{pim.formula} object
#' @param orig a locigal value indicating whether the original formula
#' (\code{TRUE}) or the \code{pim.formula} object (\code{FALSE}) should
#' be returned. Defaults to \code{FALSE}
#' 
#' @include pim-class.R 
#' @export
setGeneric("formula")

#' @export
formula.pim <- function(x, orig=FALSE, ...){
  if(orig) formula(x@formula) else x@formula
}

setMethod("formula",
          signature="pim",
          formula.pim)

#' @export
formula.pim.formula <- function(x, ...){
  x@orig
}

setMethod("formula",
          signature="pim.formula",
          formula.pim.formula)