
##This first bit of code creates a matrix and caches its inverse.
makeCacheMatrix<-function(x=matrix()){
  i<-NULL                                         ##creates matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setInverse<-function(solvedMatrix) i<<-solvedMatrix        ##sets inverse
  getInverse<-function() i                                
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

##This second bit of code either returns the cached value for the inverse of a matrix or finds the innverse of a matrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")                          ##returns cached inverse
    return(i)
  }
  data <- x$get()
  i <- solve(data)                                        ##solves matrix if no cached inverse 
  x$setInverse(i)
  i
}