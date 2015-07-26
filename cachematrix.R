## makeCacheMatrix takes a square invertible matrix and returns a 'special' 
## matrix object that can be used by the cacheSolve function to calculate
## the inverse of the matrix. cacheSolve caches the result so that if executed
## again with the same parameter passed, the result is returned from cache.
## This can help speed up execution where the inverse needs to be calculate
## repeatedly.
##
## Typical Usage:-
## > myMatrix <- matrix(c(3,9,3,2,6,1,9,7,5),3,3)
## > mySpecialMatrix <-makeCacheMatrix(myMatrix)
## > cacheSolve(mySpecialMatrix)
##

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a 'special' matrix created by using the
## function makeCacheMatrix.
##
## cacheSolve stores its result in cache. When calculating a previously
## calculated 'special' matrix the result will be returned from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## if the inverse has been previously calculated get the reult from cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## calculate the onverse of the matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}


