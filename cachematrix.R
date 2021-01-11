## Assignment is to create a pair of functions to cache and solves the inverse
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z
        list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        z <- x$getInverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        mtrx <- x$get()
        z <- solve(mtrx, ...)
        x$setInverse(z)
        z
        ## Return a matrix that is the inverse of 'x'
}