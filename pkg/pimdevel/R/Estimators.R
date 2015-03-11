#' Estimator functions for probabilistic index models
#' 
#' This page documents different possibilities for solving the score function
#' of a probabilistic index model or \code{\link{pim}}. All functions mentioned on this page, are essentially wrappers around different solver functions.
#' 
#' All functions share the same three arguments, being the design matrix \code{x}, 
#' the response vector \code{y} and the start values for the estimating function.
#' If you follow the same principles, you can write your own wrapper function 
#' for any solver function of your choice. 
#' 
#' @param x a model matrix for the respective pim model. See also 
#' \code{\link{model.matrix}}.
#' 
#' @param y a vector with the response for the respective pim model.
#' @param start a vector as long as there are columns in \code{x}, containing
#' the starting values for the algorithm
#' @param link a character vector describing the link function. ADD MORE INFO
#' @param ... extra arguments passed down to the actual solver function. See details.
#' 
#' @name estimators
#' @aliases estimator.nleqslv estimator.glm
#' @rdname estimators
estimator.nleqslv <-
  function(x,y,start=rep(0,ncol(x)), link="probit", ...){
    
    fn <- CreateScoreFun(x,y,link)
    nleqslv(start,fn, ...)
    
  }

#' @rdname estimators
estimator.glm <-
  function(x, y, start= rep(0,ncol(x)), link="probit", ...){
    
    if(is.character(link)){
      
      family <- .glmfamilies[[link]]
      
      if(is.null(family)) 
        stop("Link function",link,"not recognized by estimator.glm")
    }
    
    glm.fit(x,y, start = start, family=family, ...)
  }
