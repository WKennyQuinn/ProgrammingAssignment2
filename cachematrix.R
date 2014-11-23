## cachematrix.R contains two functions to satisfy Programming Assignment 2 of 
## Coursera course rprog-009. Topics for this assignment include lexical scoping
## and the superassignment operator (<<-). 


## makeCacheMatrix creates a list of functions for an input matrix
##  $set - set local variable to user-input matrix
##  $get - return set matrix
##  $setinverse - *must be called from cacheSolve*. Set inverted matrix
##  $getinverse - return set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <-function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix and stores it to the list 
## created by makeCacheMatrix
## *note - matrix must be square invertible*
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
        #if inverse has already been calculated, do not recalculate
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
