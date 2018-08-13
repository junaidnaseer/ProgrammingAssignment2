## *************************************************************************************************##
## Matrix inversion is usually a costly computation and their may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. The following two functions aim to achieve
## this.
##
## This file is called cachematrix.R and it contains the functions makeCacheMatrix() and 
## cacheSolve()
##
## Author: Junaid Naseer
## Date: 13.08.2018
## *************************************************************************************************##

## How to use these 2 functions:
##
## If you have a function e.g. matrixABC <- matrix(nrow=2, ncol=2, 1:4);
## first call makeCacheMatrix like this, cachedResult <- makeCacheMatrix(matrixABC)
## then call cacheSolve(cachedResult)
## and if you call caceSolve(cachedResult) again, you will see the function making use of the 
## cachedResult this time.
##


## *************************************************************************************************##
## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
## *************************************************************************************************##
makeCacheMatrix <- function(matrixToCache = matrix()) {
    matrixInverse <- NULL
    set <- function(newMatrix) {
        matrixToCache <<- newMatrixToCache;
        matrixInverse <<- NULL;
    }
    get <- function() {
        matrixToCache; 
    }
    
    setinverse <- function(inverse)  { 
        matrixInverse <<- inverse;
    }
    getinverse <- function() {
        matrixInverse;
    }
    
    cachedMatrix <- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse);
    return(cachedMatrix);
}
## *************************************************************************************************##


## *************************************************************************************************##
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## Note: Assumption is that the matrix supplied is always invertible.
## *************************************************************************************************##
cacheSolve <- function(cached_matrix, ...) {
    matrix_inverse <- cached_matrix$getinverse()
    if(!is.null(matrix_inverse)) {
        message("Inverse already calculated once. Getting cached data")
        return(matrix_inverse)          ## Return a matrix that is the inverse of 'cached_matrix'
    }
    else {
        message("no cached inverse found. Calculating inverse of matrix.");
    }
    data <- cached_matrix$get()
    matrix_inverse <- solve(data, ...)
    cached_matrix$setinverse(matrix_inverse)
    
    return(matrix_inverse);
}

## EOF
## *************************************************************************************************##