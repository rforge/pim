#' Pseudo-observation variance sandwich estimator
#' 
#' The functions described here all implement an estimator for
#' the variance of the coefficients. 
#' 
#' @param U See the formula for sandwich estimator: 
#' holds \code{U_{ij}}
#' @param U.diff See the formula for sandwich estimator: 
#' holds the partial derivatives of \code{U}.
#' @param g1,g2 Index in the original observations of the "left" and "right" part of the pseudo-observations.
#' @param shared.factor Factor by which all \code{UijUik} or
#'  \code{UijUlj} will be multiplied
#' @param switched.factor Factor by which all \code{UijUki} or
#'  \code{UijUjl} will be multiplied
#' @param self.factor Factor by which all \code{UijUij} or 
#' \code{-UijUji} will be multiplied
#' 
#' @return Some matrix. CHECK HERE!
sandwich.estimator<-function(U, U.diff, 
                             g1, g2, 
                             shared.factor=1, switched.factor=1,
                             self.factor=1, verbosity=0)
{
  #note: this estimator is greatly optimized, based on T Lumley's code!
  
  if(any(fullyequal <- g1==g2 ))
  {
    #the contribution of these terms to variance and covariance is zero anyway, because
    #I_{ii} = 0.5 always (ie: constant), so we drop them out!
    U<-U[!fullyequal,,drop=FALSE]
    g1<-g1[!fullyequal]
    g2<-g2[!fullyequal]
  }
  
  usum1.tmp <- rowsum(U,g1,reorder=FALSE)
  usum2.tmp <- rowsum(U,g2,reorder=FALSE)
  
  #TODO : optimize code below
  # use crossprod() etc.
  
  usum1  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum1.tmp),0)
  usum2  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum2.tmp),0)
  usum1 [unique(g1),] <- usum1.tmp; usum2[unique(g2),] <- usum2.tmp
  
  utushared<-((t(usum1)%*%usum1)+t(usum2)%*%usum2)
  utuswitched<-((t(usum1)%*%usum2)+t(usum2)%*%usum1)
  uDiag<-t(U)%*%U #Is counted twice as shared.factor, but needs to be counted as self.factor
  if(verbosity>0)
  {
    cat("usums:\n")
    print(cbind(usum1, usum2))
    cat("With colSums:\n")
    print(colSums(cbind(usum1, usum2)))
    cat("utushared:\n")
    print(utushared)
    cat("utuswitched:\n")
    print(utuswitched)
    cat("uDiag:\n")
    print(uDiag)
  }
  
  utu<-shared.factor*utushared  + 
    (switched.factor)*utuswitched +
    (self.factor-2*shared.factor)*(uDiag)
  
  #if the inverses occur (ij, ji), they are counted doubly as switched!!
  #However: they should be counted as - self
  #So I need all of these combinations:
  mx<-nrow(usum1)+1
  #This implementation expects mx not to be too big
  #It should work up to just about sqrt(2 ^ .Machine$double.digits)
  #which is actually above 90.000.000 on my own machine
  uids<-g1*mx+g2
  invuids<-g2*mx+g1
  invrowperrow<-match(uids, invuids)
  rowsWithInv<-(!is.na(invrowperrow)) 
  if(any(rowsWithInv))
  {
    rowsWithInv<-which(rowsWithInv)
    tmp<-sapply(rowsWithInv, function(ri){
      res<-U[ri,] %*% t(U[invrowperrow[ri],]) 
      return(res)
    })
    if(is.null(dim(tmp)))
    {
      if(length(rowsWithInv)==1)
      {
        tmp<-matrix(tmp, ncol=1)
      }
      else
      {
        tmp<-matrix(tmp, nrow=1)
      }
    }
    uijji<-matrix(rowSums(tmp), ncol=ncol(usum1.tmp))
    if(verbosity>0)
    {
      cat("uijji:\n")
      print(uijji)
    }
    
    utu<-utu-(2 *  switched.factor + self.factor)*uijji
  }
  
  #skip this last part if U.diff is identity... (note: NULL here will mean identity)
  if(is.null(U.diff)) return (utu)
  
  U.diff.inv<-solve(U.diff)
  return(U.diff.inv%*%utu%*%U.diff.inv)
}