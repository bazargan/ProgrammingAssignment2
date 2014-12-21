## There are two functions in this script file: makeCacheMatrix and cacheSolve
## makeCasheMatrix mainly creates a cached copy of a matrix and its inverse and provides a list of functions to set them
## cacheSolve calculates the inverse of the matrix and creates a cached copy of it

## makeCacheMatrix creates a cached copy of the matrix, and also provides functions for setting or modifying the stored cached matrices. 
## It also ensure that once a new matrix is set, previous solution (inversed matrix) is erased.   

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     ## initializing
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   ## list of functions to be used later
}

## cacheSolve first looks up the cached copy of the inverse matrix to output but if it does not exist, calculates the inverse and creates a cached copy for future references.   

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
                        ## Cheching the cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
