## As is inferred by the name of the function itself, the function I've written is used to 
## cache and return the inverse of the input matrix 'x'.
## This special function is able to cache potentially time-consuming computations.
## I will try to mimic and emulate the example of 'cachemean' so that the edition is handy to read, and can run smoothly.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Since it may take much time to compute the inverse of a specific matrix,
## The first step is to see if the matrix has been caculated. If so, repeatation could be avoided.
## If not, we then begin caculating.

cacheinverse <- function(x, ...) {
        m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinverse(m)
    m
}
