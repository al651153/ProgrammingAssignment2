## Matrix inversion is a time-consuming and costly computation. The following function
## involves caching the inverse of a matrix, so that instead of being repeatedly
## computed, it can be looked up in the cache to make the computation more efficient.


## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. But first, it checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. Otherwise, it computes the inverse of the matrix and sets the
## result in the cache via the setinverse function.

## Note:  This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}