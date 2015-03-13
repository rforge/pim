#' Extract response from a pim.formula
#' 
#' This function extracts the response from a 
#' \code{\link{pim.formula}} for use in \code{\link{pim.fit}}.
#' #' 
#' @param formula a \code{\link{pim.formula}} object.
#' 
#' @return The response variable with pseudo-observations for
#' a pim. 
#' 
#' @export
response <- function(formula){
  if(!inherits(formula,"pim.formula"))
    stop("formula is not a pim.formula object.")
  eval(lhs(formula), env=penv(formula))
}