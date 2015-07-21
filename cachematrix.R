## Put comments here that give an overall description of what your
## functions do:

## Computing the inverse of a matrix is potentially very time consuming.
## Two functions are introduced here to cache its value and don't compute it 
## again if it is not needed. makeCacheMatrix creates the matrix and cacheSolve
## is used to check if there is there is already a cached value or compute the
## inverse if it has not been yet.
## It is assumed that the matrix supplied is always invertible.


## Write a short comment describing this function:
## This function creates a special "matrix" object that can cache its inverse.
## It is actually a list with 4 functions to set and get the matrix and its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
