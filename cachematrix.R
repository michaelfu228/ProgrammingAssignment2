## This file includes a pair of functions to cache the inverse of a matrix

## This first function is meant to create a special "matrix" object as a list of 4 functions to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(z) {inv <<- z}
    get_inv <- function() inv
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    if(!is.null(x$get_inv())) {
        print("getting cache data")
        return(x$get_inv())
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
