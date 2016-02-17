# ##############################################################################
# Coursera - R Programming - Programming assignment 2
# The functions calculate the inverse of a matrix and store that value in the cache
# of a special "matrix" object. The special matrix also includes functions to retrieve
# the inverse matrix, reset the original matrix and output the original matrix.
# ##############################################################################


## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
      
}
