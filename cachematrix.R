## Caching the Inverse of a Matrix

## The Function makeCacheMatrix has:
## INPUT: 1 Matrix (Assumed Invertible)
## OUTPUT: A list of 4 functions that will:
##              set(y) - Sets the matrix Value as y
##              get() - Get the Matrix X
##              setinverse(inverse) - Sets the inverse value for the matrix
##              getinverse - Get the inverse for the Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- as.matrix(y)
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This Function Checks if the inverse of the Matrix has been cached
## If so it returns that value (without recalculating), 
## if not it will calculate it and cache it using the makeCacheMatrix function
## INPUT: 1 makeCacheMatrix (Result of using the function makeCacheMatrix with the Matrix)
## OUTPUT: the inverse of the Matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



