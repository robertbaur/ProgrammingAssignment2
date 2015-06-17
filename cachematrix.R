## The following pair of function saves time if a the inverse of 
## a matrix must be computed more than once. If this is the case
## the inverse will be retrieved from the cache instead of being 
## calculated again.

## makeCacheMatrix calculates the inverse of a matrix and stores 
## it in the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## The cacheSolve function checks whether the inverse of the matrix
## created by makeCacheMatrix has already been calculated. If it has
## already been calculated the function 
## retrieves the previously computed inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setInverse(m)
  m
}