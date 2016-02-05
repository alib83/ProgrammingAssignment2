## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse after it is calculated 
## the first time

## makeCacheMatrix creates a special "matrix", which is really a list containing
## 4 functions to set / get the matrix and to set / get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL 
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of a special "matrix" which was created
## with the makeCacheMatrix function. If the inverse was already calculated,
## it gets the inverse rather than compute it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
