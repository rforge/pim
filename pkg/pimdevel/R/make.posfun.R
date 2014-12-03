#' Create a poset function
#' 
#' This function creates a poset function from a poset.
#' 
#' @param poset a vector with the columns as indices
#' 
#' @return A function that takes a single vector as argument, and that
#' returns the vector with the poset vector applied to it.
#' 
.make.posfun <- function(poset, class='posetfun'){
  # Sanity checks
  if(!is.numeric(poset))
    stop("poset has to be numeric")
  
  if(!is.vector(poset))
    stop("poset should either be a matrix or a vector.")
  
  poset <- as.integer(poset)
  
  function(x){
    x <- x[poset]
    structure(x, class = unique(c(class, oldClass(x))))
  }
}
