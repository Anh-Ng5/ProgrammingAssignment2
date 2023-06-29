## Caching the inverse of a matrix

## Write a pair of functions that cache the inverse of a matrix instead of computing it repeatedly.
## These functions aim to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of the matrix created with the makeCacheMatrix function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
## and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
