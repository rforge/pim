PIM.Data <-
function(data)
{
	n <- nrow(data)
	d <- ncol(data) - 1
	data <- data[do.call(order, as.data.frame(data[,2:(d+1)])),]
	left.tmp <- unlist(mapply(seq,rep(1,(n-1)), 1:(n-1)))
	right.tmp <- unlist(mapply(rep, 2:n, 1:(n-1)))
	data.tmp <- as.data.frame(matrix(nrow=n*(n-1)/2, ncol=(1+2*d+2)))
	names(data.tmp) <- c("PO",paste(names(data)[-1],"L",sep=""),paste(names(data)[-1],"R",sep=""),"groupL","groupR")
	data.tmp[,1] <- c(data[left.tmp,1] < data[right.tmp,1]) + 0.5*c(data[left.tmp,1] == data[right.tmp,1])
	data.tmp[,2:(d+1)] <- data[left.tmp,-1]
	data.tmp[,(d+2):(2*d+1)] <- data[right.tmp,-1]
	data.tmp[,2*d+2] <- left.tmp
	data.tmp[,2*d+3] <- right.tmp
	return(data.tmp)
  }

