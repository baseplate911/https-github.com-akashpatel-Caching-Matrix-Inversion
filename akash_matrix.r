## Matrix Inversion Cache Implementation
## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getInverse <- function() inv
    
    # Return list of functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute and cache the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    # If inverse is cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise calculate inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}
