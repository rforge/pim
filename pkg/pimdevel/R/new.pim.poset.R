#' Create a pim.poset environment
#' 
#' This function allows you to create a \code{\link{pim.poset}} environment 
#' that can be added to a \code{\link{pim.environment}} object. You can
#' use this function to create a custom poset, but in general it's safer
#' to use the relevant arguments of the \code{\link{pim}} function. 
#' That way more safety checks are carried out.
#' 
#' @details A poset (or partially ordered set) in the context of 
#' probabilistic index models is a set of indices that determines
#' which observations are compared with one another. It is used to
#' construct the pseudo-observations on which the model is fitted.
#' You can think of a poset as a "pseudo-observation set".
#' 
#' The most convenient way to use this function, is by specifying
#' a character value for the argument \code{compare}. The value "unique"
#' creates a poset in such a way that only unique combinations of two
#' observations are used in the model. The value "all" creates all
#' possible L-R combinations between the observations.
#' 
#' If you want to define the poset yourself, you can pass either a matrix 
#' or a list with 2 elements as value for the argument \code{compare}.
#' Columns of the matrix or elements of the list should either be named
#' "L" and "R", or be unnamed. When unnamed, the function takes the first
#' column/element as the left poset, and the second column/element as
#' the right poset.
#' 
#' @param compare A character value, matrix or list indicating how the
#' poset should be constructed. Defaults to the default value of 
#' \code{\link{create.poset}}. See Details section for more information.
#' 
#' @param nobs An integer value determining the number of observations
#' this poset is created for. If compare is not a character value,
#' the number of observations 
#' 
#' @param parent An environment that serves as the parent for the
#' \code{pim.poset} environment. By default this is the environment
#' from which the function is called. Note that for a correct functioning,
#' the parent environment should be set to the \code{\link{pim.environment}}
#' this object is part of. This is done automatically by the function
#' \code{\link{add.poset}}.
#' 
setGeneric("new.pim.poset",
           function(compare,nobs,parent=parent.frame(),...){
             standardGeneric("new.pim.poset")
           })

setMethod("new.pim.poset",
          signature=c(compare="character",
                      nobs="numeric"),
          function(compare,nobs,parent,...){
            poset <- create.poset(compare,n=nobs)
            out <- new.pim.poset(poset,nobs,parent,...)            
            out@compare <- compare
            out
          }
          )

setMethod("new.pim.poset",
          signature=c(compare="matrix",
                      nobs="numeric"),
          function(compare,nobs,parent,...){
            poset <- lapply(seq_len(ncol(compare)),
                            function(i) x[,i])
            names(poset) <- colnames(compare)
            new.pim.poset(poset,nobs,parent,...)
          })

setMethod("new.pim.poset",
          signature=c(compare="list",
                      nobs="numeric"),
          function(compare,nobs,parent,...){
            
            if(length(compare) != 2L )
              stop("Compare should contain exact 2 columns/elements")
            names <- names(compare)
            if(is.null(names)){
              names(compare) <- c("L","R")
            } else if(!all(match(c("L","R"),names,0L) > 0)){
              stop("Names don't match L and R")
            }
            out <- new("pim.poset")
            parent.env(out) <- environment()
              # This makes sure the object compare can be found
              # 
            out@compare <- "custom"
            out@nobs <- as.integer(nobs)
            
            
            eval(quote({
              L <- .make.posfun(compare[['L']])
              R <- .make.posfun(compare[['R']])
            }),envir=out)
            parent.env(out) <- parent
            out
          })

setMethod("new.pim.poset",
          signature=c(compare="matrix",
                      nobs="missing"),
          function(compare,parent,...){
            nobs <- max(compare)
            new.pim.poset(compare,nobs,parent,...)
          })

setMethod("new.pim.poset",
          signature=c(compare="list",
                      nobs="missing"),
          function(compare,parent,...){
            nobs <- max(.Internal(unlist(compare,FALSE,FALSE)))
            new.pim.poset(compare,nobs,parent,...)
          })
