## The functions cache the inverse of a matrix, increasing the efficiency of the process
## involved in matrix inversion

## The function sets the value of a matrix and gets this value. Then, the function sets
## the value of inverse of the matrix and gets this value.

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

## The function returns the inverse of the matrix. First it determines if the inverse
## has been computed and gives the result. If the inverse has not been computed, 
## the function sets the value in the cache.

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

