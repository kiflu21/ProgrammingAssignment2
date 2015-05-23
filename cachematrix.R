### Caching the Inverse of a Matrix
## General Overview:
# Matrix inversion is usually a costly computation and there may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly 
# Below is a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: This is the first function that creates a special "matrix" object 
##    that can cache its inverse. It creates a list containing a function used to:
# a. set the value of the matrix
# b. get the value of the matrix
# c. set the value of inverse of the matrix
# d. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## 2. cacheSolve: This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above.This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
