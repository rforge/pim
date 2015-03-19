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
#' @seealso \code{\link[nleqslv]{nleqslv}}, \code{\link[stats]{glm.fit}},
#' \code{\link[BB]{BBsolve}} for more information on the fitting
#' algorithms.
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
#' @return a list with following elements:
#'  \item{coef}{the estimated coefficients}
#'  \item{conv}{information on the convergence. This isn't 
#'  implemented yet.}
#' 
#' @import nleqslv
#' @name estimators
#' @aliases estimator.nleqslv estimator.glm
#' @rdname estimators
#' @export
estimator.nleqslv <-
  function(x,y,start=rep(0,ncol(x)), link="logit", ...){
    
    fn <- CreateScoreFun(x,y,link)
    res <- nleqslv(start,fn, ...)
    
    if(res$termcd != 1){
      warning(paste("nleqslv says:", res$message,"\n",
                    "See ?nleqslv for more info."),
              call. = FALSE)
    }
    list(coef = res$x)
    
  }

#' @rdname estimators
#' @export
estimator.glm <-
  function(x, y, start= rep(0,ncol(x)), link="logit", ...){
    
    if(is.character(link)){
      
      family <- .glmfamilies[[link]]
      
      if(is.null(family)) 
        stop("Link function",link,"not recognized by estimator.glm")
    }
    
    res <- withCallingHandlers(
      glm.fit(x,y, start = start, family=family, ...),
      warning = catch.noninteger.handler
    )
    list(coef = res$coef)
  }

#' @rdname estimators
#' @import BB
#' @export
estimator.BB <-
  function(x, y, start= rep(0,ncol(x)), link="logit", 
           control=list(NM = c(FALSE,TRUE),
                        method = c(1,2,3)), ...){
    fn <- CreateScoreFun(x,y,link)
    res <- BBsolve(start,fn, ...)
    
    if(res$convergence != 0 ){
      warning(paste("BBsolve says:", res$message, "\n",
                    "See ?BBsolve for more info."),
              call. = FALSE)
    }
    
    list(coef = res$par)
  }