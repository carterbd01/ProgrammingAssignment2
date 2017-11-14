makeCacheMatrix<-function(x=matrix()){
  i<-NULL                                         ##creates matrix
  set<-function(y){
    x<<-y
    Inverse<<-NULL
  }
  get<-function() x
  setInverse<-function(solvedMatrix) i<<-solvedMatrix
  getInverse<-function() i                                ##sets inverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
cacheInverse <- function(x, ...) {
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