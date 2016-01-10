## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix(matrix m) takes a matrix as input and creates a special CacheMatrix object
## that can cache its inverse.
## cacheSolve(cacheMatrix) takes a CacheMatrix object as input and checks whether the inverse has
## already been computed and cached.  If so, it returns the cached inverse.  Otherwise, it solves
## for the inverse, sets the CacheMatrix inverse to the solution found, and returns the solution found.


## Write a short comment describing this function
## makeCacheMatrix(matrix m) creates the CacheMatrix object, setting the matrix to the parameter passed to
## the function (or an empty matrix by default) and the inverse to NULL.
## It provides fourt functions for getting and setting the inverse, and getting the original matrix, as well
## as for getting the object type for checking by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    mat <- x
    inv <- NULL
    setinv <- function(y) {
        inv <<- y
    }
    getinv <- function() inv
    getmatrix <- function() mat
    getobject <- "CacheMatrix"
    list(setinv = setinv, getinv = getinv, getmatrix = getmatrix, getobject = getobject)
}


## Write a short comment describing this function
## cacheSolve(CacheMatrix) first checks whether the object passed as the first object is a CacheMatrix.  
## If it is not, then the function returns NA with an error message.
## Otherwise, it obtains the inverse of the CacheMatrix object passed.  If this is NULL, i.e. the inverse has not
## yet been calculated, it solves for the inverse (getting the matrix from the parameter passed) and stores it in the
## CacheMatrix object.  It then returns the inverse obtained or newly computed.

cacheSolve <- function(x, ...) {
    if (is.na(x["getobject"]) | !identical(x["getobject"][[1]],"CacheMatrix")) {
        message("ERROR: Object is not a CacheMatrix")
        return(NA)
    } 
    inv <- x$getinv()
    if (is.null(inv)) {
        message("No cached inverse - running solve()")
        inv <- solve(x$getmatrix(),...)
        x$setinv(inv)
    } else {
        message("Returning cached inverse")
    }
    inv
}
