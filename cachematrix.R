## These functions allow you to cache the inverse of a matrix
## and access that inverse from other functions

## makeCacheMatrix() provides a set of functions for storing 
## and access the initial matrix as well as the inverse once
## the inverse has been solved vie cacheSolve() or via direct 
## assignment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setinv <- function(inverse)  inv <<- inverse
    getinv <- function() inv
    list(get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() accepts a makeCacheMatric() object and either
## solves, stores and returns a newly computed inverse or simply
## returns an inverse if already cached
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
