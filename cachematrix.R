## Put comments here that give an overall description of what your
## functions do

## The first two lines initialize the object.  The set function comes into
## play only if you want to evaluate a new matrix, then you "set" the new data
## using a$set(c( , , ,)) without having to reinitialize. See "setters and
## getters" or mutator and accessor.

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This portion will check to see if the inverse of the matrix exists in the
## cache already (that is the if statement).  If not, it computes the inverse
## in the i <- solve() function call.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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