## The following functions provide a means to cache the inverse of a matrix for repeated access
## to improve loop performance.

## makeCacheMatrix(): Function
## Creates a list of functions to get/set the value of a matrix and cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }

    ## Return current maxtrix "x"
    get <- function() x
    
    ## Update the xinverse cache value
    setinverse <- function(inv) xinverse <<- inv
    
    ## return the xinverse cache value
    getinverse <- function() xinverse
    
    ## return list of the functions created above
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve(): Function
## Calculates the inverse of matrix "x" if it has not already been done, retrieves
## the inverse from cache if it has.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if we've already calculated the inverse
    xinverse <- x$getinverse()
    if(!is.null(xinverse)){
        message("Inverse retrieved from cache.")
        ## return cached value
        return(xinverse)
    }
    
    ## We haven't already calcuated the inverse, do so now
    matrixdata <- x$get()
    xinverse <- solve(matrixdata, ...)
    
    ## update the cache value
    x$setinverse(xinverse)
    
    ## return calculated value
    xinverse
}