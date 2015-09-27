# This script finds the inverse of any invertible matrix given to the function

# makeCacheMatrix creates a list containing a function to set/get the value of
# the matrix and set/get the value of inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


# This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# cacheSolve assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
        message("getting cached data.")
        return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
