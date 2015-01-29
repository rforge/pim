#' Probability function
#' 
#' This functions transform a comparison or otherwise
#' logical value to a numeric value for use in a pim.
#' 
#' These functions are constructed purely for notation. 
#' \code{P} is completely equivalent
#' to \code{\link{as.numeric}}, apart from an extra control
#' to check whether it actually makes sense to do so. 
#' The function \code{PO} is just short for \code{P(x < y) + 0.5*P(x == y)}
#' 
#' @param x for \code{P}, a logical value. For \code{PO} a numeric value.
#' @param y a numeric value
#' 
#' @return either 0 or 1
#' 
#' @examples
#' # Check in pim
#' 
#' @aliases PO
#' @export
P <- function(x){
  if(is.logical(x)) as.numeric(x) else 
    stop("P requires logical values.")
}

#' @rdname P
#' @export
PO <- function(x,y){
  P(x < y) + 0.5*P(x == y)
}