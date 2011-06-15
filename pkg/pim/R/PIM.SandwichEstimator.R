PIM.SandwichEstimator <-
function(U, U.diff, g1, g2)
{
	if (!exists("rowsum")) require(survival4)
	usum1.tmp <- rowsum(U,g1,reorder=F)
	usum2.tmp <- rowsum(U,g2,reorder=F)
	usum1  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum1.tmp),0)
	usum2  <- matrix(nrow=length(union(g1,g2)),ncol=ncol(usum2.tmp),0)
	usum1 [unique(g1),] <- usum1.tmp; usum2[unique(g2),] <- usum2.tmp
	utu<-(t(usum1)%*%usum1)+t(usum2)%*%usum2  + (t(usum1)%*%usum2) + (t(usum2)%*%usum1) -t(U)%*%U
        U.diff.inv <- solve(U.diff)
	return(U.diff.inv%*%utu%*%U.diff.inv)
}

