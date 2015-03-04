#' Constructor for pim.formula
#' 
#' This function reworks a formula to a pim.formula for use
#' in a probabilistic index model. This function is only meant
#' to be used internally, so it's not exported.
#' 
#' @param formula a formula object
#' @param data either a \code{\link{pim.environment}} object containing the
#' data for the pim, or an object that can be converted to a 
#' \code{pim.environment} by \code{\link{new.pim.env}}
#' @param ... extra arguments to \code{\link{new.pim.env}}
#' 
#' @return a \code{\link{pim.formula}} object.
#' 
setGeneric("new.pim.formula",
           function(formula, data, ...) standardGeneric("new.pim.formula"))

setMethod("new.pim.formula",
          signature=c("formula","pim.environment"),
          function(formula, data){
            environment(formula) <- data
            ft <- terms(formula, simplify=TRUE)
            formula <- formula(ft)
            
            lhs <- formula[[2]]
            rhs <- formula[[3]]
            
            response <- all.vars(lhs)
            predictors <- all.vars(rhs)
            vars <- union(response, predictors)
            
            funs.rhs <- setdiff(all.names(rhs,unique=TRUE),
                                predictors)
            funs.lhs <- setdiff(all.names(lhs,unique=TRUE),
                                predictors)

            has.funs.lhs <- any(match(funs.lhs,.specials.pim.lhs,0L) >0L  )
            has.specials <- any(match(funs.rhs,.specials.pim.rhs,0L) >0L  )
            
            # THIS WILL CHANGE WHEN SURV/HIST WILL BE USED
            has.lhs.fun <- FALSE
            
            if(!has.funs.lhs)
              rhs <- as.language(paste("PO(L(",response,"),R(",
                                       response,"))"))
            
            out <- new("pim.formula",
                terms = ft,
                has.specials = has.specials,
                has.lhs.fun = has.lhs.fun,
                lhs = enquote(lhs),
                rhs = enquote(rhs),
                penv = data)
            out
          })

setMethod("new.pim.formula",
          signature=c("formula","ANY"),
          function(formula,data, ...){
            data <- new.pim.env(data, ...)  
            new.pim.formula(formula,data)
          })