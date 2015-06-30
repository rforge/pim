#' Getters for slots of a pim object
#' 
#' @param x a pim object
#' 
#' @rdname pim-getters
#' @aliases keep.data
#' 
#' @return \code{keep.data()}: a single logical value indicating whether
#' the model matrix and pseudo responses were stored in the 
#' \code{\link{pim}} object.
#' 
#' @include pim-class.R
#' @export
keep.data <- function(x){
  if(!inherits(x,"pim"))
    stop("Object is not a pim object")
  else
    x@keep.data
}