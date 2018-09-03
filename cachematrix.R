## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special object (list of functions)
## that can cache the inverse of its stored matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL

    set <- function(y) {
            x <<- y
            s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s

    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve calculates the inverse of the matrix stored in the special
## object created in makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    s <- x$getsolve()
    
    if(!is.null(s)) {
            message("getting cached data")
            return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    
    s    ## Return a matrix that is the inverse of 'x'

}
