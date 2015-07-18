## R Programming - Assignment 2
## Completed 2015-07-17
## By Steven Peterson
##
## These functions implement functionality to cache the calculation
## of a matrix inversion.
##
## makeCacheMatrix creates an object that caches
## a result.
##
##  x - is a numeric matrix
##
##  The return value is a list of functions that implement
##  accessors for the data and the cache value.
##
makeCacheMatrix <- function(x = matrix()) {
    
    ## Private variable holding the cached value.
    cacheValue <- NULL
    
    ## Set accessor - set the source matrix
    set <- function(y) {
        x <<- y
        cacheValue <<- NULL
    }
    
    ## Get accessor - get the source matrix
    get <- function() x
    
    ## Set accessor for the cached value
    setCacheValue <- function(value) cacheValue <<- value
    
    ## Get accessor for the cached value
    getCacheValue <- function() cacheValue
    
    ## Return a list that contains the functions implementing
    ## the objects methods.
    list(
        set = set,
        get = get,
        setCacheValue = setCacheValue,
        getCacheValue = getCacheValue
    )
}
##
## cacheSolve computes the inverse of a
## cacheMatrix object.
##
##  x is a cacheMatrix object created with the method makeCacheMatrix
##
##  The return value is a matrix containing the computed inverse.
##
## Note: cacheSolve optimizes the calculation by pulling
## from a cached solution, only computing the inverse
## if the cached copy does not exist.
cacheSolve <- function(x, ...) {

    ## Pull the result from the cache.
    result <- x$getCacheValue()
    
    if (is.null(result))
    {
        ## if the cached value does not exist...
        ##   1. Compute the inverse
        ##   2. Cache the result for the next call
        d <- x$get()
        result <- solve(d, ...)
        x$setCacheValue(result)
    }
    else
    {
        message("getting cached data")
    }
    
    ## Return the result.
    result
}
