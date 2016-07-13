## The two functions work together to calculate and return an inverse matrix to the one inputed by a user
## A sample call for the functions would be:
## > cacheSolve(makeCacheMatrix(x = matrix(rnorm(16),4,4)))

## This function creates a list with four functions: to set the actual matrix, to get its values based on the input, 
## to set an inverse matrix, to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the inverse of the actual matrix already exists and returns it. 
## If the inverse matrix doesn't exists, it calculates it and returns it.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
