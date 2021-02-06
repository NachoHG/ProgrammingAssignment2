## This script performs a computation of the inverse of a matrix. As the higher
## dimension, the higher computer cost it can have, the function stores the
## inverse in case it has been previously computed.

## This function gets and stores the matrix and, if already computed, its inverse.

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


## This function tries to get the inverse of a matrix if it has been previously
## computed -stored-. Otherwise, it calculates it by using the function solve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
