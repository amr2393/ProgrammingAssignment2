## Functions to cache the inverse of a matrix to avoid time consuming computations where possible

## Initialize matrix structure
## Provides methods to set and get matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to NULL
  inv <- NULL
  
  # Set function to update value of matrix
  # Sets inverse to NULL to ensure it must be recomputed when matrix is changed
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get function - returns matrix
  get <- function() x
  
  # Set inverse of matrix to given value
  setinv <- function(inverse) inv <<- inverse
  
  # Get inverse value 
  getinv <- function() inv
  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  # Get inverse stored in x
  inv <- x$getinv()
  
  # Check that cached value is not-null, otherwise retrieve cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Otherwise, get matrix and invert it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Update inverse in the cacheMatrix
  x$setinv(inv)
  # Return inverse
  inv
}
