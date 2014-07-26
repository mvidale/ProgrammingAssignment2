## Given a matix m, makeCacheMatrix(m) will return a matrix object (mo) that can 
## cache its own inverse. This object can then be used by the cacheSolve(mo) 
## function to either return the cached inverse if it exists or calculate the
## inverse and then add it to the cache.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function takes the special matrix object and returns the cached inverse
## of that matrix if it exists or calculates the inverse and then stores it in
## in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
