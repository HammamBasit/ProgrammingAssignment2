## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Below is a pair of functions 
## that cache the inverse of a matrix.

## makeCacheMatrix is a function that will returns a special matrix
## which can be accessed through cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  ## initialize
  inverse <- NULL
  ## set() is used to setup the cached matrix.
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  ## get() returns the input matrix 'x'
  get <- function() x
  ## setInverse() is to set the inverse of x to cache.
  setInverse <- function(inverseIn) inverse <<- inverseIn
  ## getInverse() returns the cached inverse of x.
  getInverse <- function() inverse
  
  ## return a list of functions that will be used by cacheSolve()
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function will return a matrix that is the inverse of x.
## x is the special matrix returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## try to get the inverse of x from cache.
  inverse <- x$getInverse()
  ## if the cache is valid...
  if (!is.null(inverse))
  {
    ## return data from cache!
    message("getting cached data")
    return(inverse)
  }
  ## if the cache contains nothings...
  ## lets get the matrix to compute the inverse
  data <- x$get()
  ## compute the inverse
  inverse <- solve(data, ...)
  ## set the inverse to cache
  x$setInverse(inverse)
  ## return the inverse
  inverse
}

