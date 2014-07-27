## Following are a pair of functions to calculate the inverse of a matrix 
## and store it in a cache and retrieve it from there.

## This creates a special vector which is a list of functions which get/sets
## the value of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
      setmatrix <- function(y) {
      	x <<- y
            inverse <<- NULL
      }
      getmatrix <- function() x
      setinverse <- function(m_inverse) inverse <<- m_inverse
      getinverse <- function() inverse
      list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This takes the special vector created by above function as input produces
## the inverse of matrix either from cache if the inverse is already 
## calculated and matrix has not changed. Otherwise it calculates the inverse
## and stores it in the cache.
 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
      if(!is.null(inverse)) {
      	message("getting cached inverse data")
            return(inverse)
      }
      data <- x$getmatrix()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse	
}

