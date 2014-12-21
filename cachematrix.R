## *************************************************************************************************##
## Matrix inversion is usually a costly computation and their may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. The following two functions aim to achieve
## this.
##
## This file is called cachematrix.R and it contains the functions makeCacheMatrix() and 
## cacheSolve()
##
## Author: Junaid Naseer
## Date: 21.12.2014
## *************************************************************************************************##


## *************************************************************************************************##
## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
## *************************************************************************************************##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## *************************************************************************************************##


## *************************************************************************************************##
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## Note: Assumption is that the matrix supplied is always invertible.
## *************************************************************************************************##
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                                            ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## EOF
## *************************************************************************************************##