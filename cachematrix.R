
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse to NULL
        inv <- NULL
        
        # Function to set the matrix value and invalidate the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to get the current matrix
        get <- function() x
        
        # Function to set the cached inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # Function to get the cached inverse
        getInverse <- function() inv
        
        # Return a list of functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # If the inverse is cached, retrieve and return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse is not cached, calculate it
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # Cache the calculated inverse
        x$setInverse(inv)
        
        # Return the calculated inverse
        inv
}
