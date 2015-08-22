## This library provides two functions. One to make a special kind of
## matrix that provides getters and setters for a matrix and
## its inverse. The second function is a smart inverse function
## that can cache previously computed inverse and return it on
## subsequent calls.

## This function provides a list of four functions. The functions
## get() and set() return and set the matrix x. The functions
## getInverse() and setInverse() return and set inverse of x.
## Argument to this function must be a matrix ot it will be coersed
## into a matrix. The matrix is implicitly assumed to be invertible.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function that returns the inverse of the matrix. It computes the
## inverse only if it is not computed already. If the inverse was
## already computed, the cached value is returned instead of recomputing
## it again.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
