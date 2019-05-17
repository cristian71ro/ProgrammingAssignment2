## The functions below are used to create a matrix object that can cache its inverse (1) and compute the inverse of the matrix 
## object (2)

## makeCacheMatrix() is a function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setval <- function(z) invm <<- z
    getval <- function() invm
    list(set=set, get=get, setval=setval, getval=getval) 
}


## cacheSolve() function computes the inverse of the matrix object created with makeCacheMatrix()
## cacheSolve() uses another function: solve() - computes the inverse of a square matrix
## the matrix used to build the matrix object should be inversible (square matrix with determinant!=0)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getval()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setval(invm)
    invm
}
