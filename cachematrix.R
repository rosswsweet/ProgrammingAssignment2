## This code was written for the Week 3 Peer-graded Assignment in the R Programming course
## on Coursera.
## This code defines two functions, makeCacheMatrix and cacheSolve.
## Together, these functions compute, cache, and retrieve the inverse of an
## invertible matrix. makeCacheMatrix is called first on a matrix x, then cacheSolve is
## called on the output of makeCacheMatrix.
## This code was developed by Ross Sweet.

## The makeCacheMatrix function takes as an input an invertible matrix, x,
## and outputs a list of four functions:
##  set: redefines the matrix x and sets the inverse to NULL
##  get: prints the matrix x
##  setinv: assigns to inv the value of the inverse of x
##  getinv: prints the inverse matrix inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve prints the inverse of x.
## First, cacheSolve tries to retrieve inv using the getinv function from makeCacheMatrix.
## If the value of inv is not NULL, cacheSolve prints the cached value of inv.
## If the value of inv is NULL, cacheSolve retrieves the value of x from makeCacheMatrix,
## computes the inverse using solve(), caches the value of inv using makeCacheMatrix,
## then prints inv.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
