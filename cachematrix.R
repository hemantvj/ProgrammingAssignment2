## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  temp <- matrix(rep(NA,nrow(x)*ncol(x)),nrow(x),ncol(x))
  set <- function(y){
      x <<- y
      temp[,] <<- NA
  }
  get <- function() x
  getinverse <- function() temp
  setinverse <- function(inv) temp <<- inv
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  b <- x$getinverse()
  if(!is.na(b[1,1])){
    message("getting cached inverse matrix")
    return(b)
  }      
  data <- x$get()
  b <- solve(data, ...)
  x$setinverse(b)
  message("inverse calculated and cached")
  b
  ## Return a matrix that is the inverse of 'x'
}
