## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix passed by argument
## get the value of the original matrix 
## setSolve the value of the inverse matrix calculated by the solve function
## getSolve the value of the inverse matrix saved in the object

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
    set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inverseMatrix <<- solve
    getSolve <- function() inverseMatrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve calculates the inverse of the special "matrix" x
# First of all, it checks if the inverse matrix has been calculated
# previously. If so, it gets the inverse matrix from the cache. Else,
# it calculates the inverse of x by taking the original matrix and
# applying the solve function, setting its value in the cache by the 
# setSolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverseMatrix <- x$getSolve()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setSolve(inverseMatrix)
    inverseMatrix
}
