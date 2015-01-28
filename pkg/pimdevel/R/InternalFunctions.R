# Internal functions
# 
# These are a number of convenience functions that are used internally
# 

# lpaste pastes names together as a "list" to be used in a message
# argument x: a character vector
.lpaste <- function(x){
  lx <- length(x)
  if(!is.character(x)){
    stop("lpaste can only paste character values.")
  } else if(lx < 2){
    return(x)
  } else if(lx == 2){
    return(paste(x,collapse=" and "))
  } else{
    tmp <- paste(x[-lx], collapse=", ")
    return(paste(tmp,x[lx],sep=" and "))
  }
}

# Checks whether x and y contain the same elements
.same.elements <- function(x,y){
  if(!is.vector(x) || !is.vector(y))
    stop("x and y should be vectors.")
  
  !any(
    match(x,y,0L) == 0L,
    match(y,x,0L) == 0L
    )
  
}

# Gets the classes of all objects in an environment
# Takes an environment as 
.get.classes <- function(envir){
  all.names <- ls(envir)
  all.classes <- lapply(mget(all.names,envir=envir),
         class)
  names(all.classes) <- all.names
  all.classes
}

# Checks the classes in an environment against a (named)
# vector or list with the classes mentioned.
# 
.same.classes <- function(envir,classes){
  all.classes <- .get.classes(envir)
  if(!is.list(classes)) classes <- as.list(classes)
  
  if(!is.null(names.classes <- names(classes))){
    if(!.same.elements(names.classes,names(all.classes))){
      stop("Names of classes don't match")
    } else {
      all.classes <- all.classes[names.classes]
    }
  }
  identical(all.classes, classes)
}

.equal.nobs <- function(envir){
  all.lengths <- sapply(ls(envir), function(i){
    nobs(get(i,envir=envir,inherits=FALSE))
  })
  length(unique(all.lengths)) <= 1
}

valid.classes <- function(x){
  all(match(x,.valids.pim,0L) > 0)
}

# Currently not used. valid.classes should do it
# Could be adapted 
is.variable <- function(x){
  is.vector(x) | inherits(x, 'factor')
}